use crate::chunks::{AudioData, AudioDescription, Chunk, ChunkHeader, PacketTable};
use crate::{ChunkType, CAF_HEADER_MAGIC, CAF_HEADER_MAGIC_LEN};
use std::borrow::Cow;
use std::error::Error;
use std::fmt::Display;
use std::io::{Error as IoError, Read, Seek, SeekFrom, Take};
use std::string::FromUtf8Error;

/// An error related to reading.
#[derive(Debug)]
pub enum CafReadError {
    /// Wrapped I/O error.
    Io(IoError),
    /// Can't decode UTF-8.
    FromUtf8(FromUtf8Error),
    /// Output buffer is too small.
    BufferSize,
    /// Stream doesn't start with a CAF header.
    NotCaf,
    /// Chunk type is not supported.
    UnsupportedChunkType(ChunkType),
    /// Invalid data.
    InvalidData(Cow<'static, str>),
}

impl CafReadError {
    fn description_str(&self) -> &str {
        match &self {
            CafReadError::Io(_) => "I/O error",
            CafReadError::FromUtf8(_) => "Can't decode UTF-8",
            CafReadError::BufferSize => "Output buffer is too small.",
            CafReadError::NotCaf => "Stream doesn't start with a CAF header",
            CafReadError::UnsupportedChunkType(_) => "Chunk type is not supported",
            CafReadError::InvalidData(s) => s,
        }
    }
}

impl Error for CafReadError {
    fn description(&self) -> &str {
        self.description_str()
    }

    fn cause(&self) -> Option<&dyn Error> {
        match self {
            CafReadError::Io(ref err) => Some(err as &dyn Error),
            CafReadError::FromUtf8(ref err) => Some(err as &dyn Error),
            _ => None,
        }
    }
}

impl Display for CafReadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Self::description_str(self))
    }
}

impl From<IoError> for CafReadError {
    fn from(err: IoError) -> CafReadError {
        CafReadError::Io(err)
    }
}

impl From<FromUtf8Error> for CafReadError {
    fn from(err: FromUtf8Error) -> CafReadError {
        CafReadError::FromUtf8(err)
    }
}

macro_rules! read_invalid {
    ($text:expr) => {
        CafReadError::InvalidData(std::borrow::Cow::Borrowed(concat!("Invalid data: ", $text)))
    };
}

macro_rules! invalid_chunk_size {
    ($chunk:expr) => {
        read_invalid!(concat!("invalid chunk size for ", $chunk, " chunk"))
    };
}

pub(crate) use invalid_chunk_size;
pub(crate) use read_invalid;

/// Reader for chunks from a CAF stream.
pub struct ChunkReader<T: Read> {
    rdr: T,

    audio_description: Option<AudioDescription>,
}

impl<T: Read> ChunkReader<T> {
    /// Constructs a new [`ChunkReader`] from a given [`Read`].
    pub fn new(mut rdr: T) -> Result<Self, CafReadError> {
        let mut header_buf = [0; 8];
        rdr.read_exact(&mut header_buf)?;
        if header_buf != CAF_HEADER_MAGIC {
            return Err(CafReadError::NotCaf);
        }

        Ok(ChunkReader {
            rdr,
            audio_description: None,
        })
    }

    /// Returns the wrapped reader, consuming the [`ChunkReader`].
    pub fn into_inner(self) -> T {
        self.rdr
    }
    /// Returns a reference to the wrapped reader.
    pub fn get_ref(&self) -> &T {
        &self.rdr
    }
    /// Returns a mutable reference to the wrapped reader.
    pub fn get_mut(&mut self) -> &mut T {
        &mut self.rdr
    }

    /// Reads a chunk header.
    ///
    /// Returns `None` if the end of the stream was reached.
    pub fn read_chunk_header(&mut self) -> Result<Option<ChunkHeader>, CafReadError> {
        ChunkHeader::read(&mut self.rdr)
    }

    /// Reads a chunk's body and decodes it.
    /// The length of the chunk's body is taken from the header.
    pub fn read_chunk_body(&mut self, header: &ChunkHeader) -> Result<Chunk, CafReadError> {
        let chunk = Chunk::read_body(&mut self.rdr, header, self.audio_description.as_ref())?;

        // if we just read the Audio Description chunk, store it, since the Packet Table chunk needs it
        if let Chunk::AudioDescription(audio_description) = &chunk {
            // TODO: maybe it's possible to remove this clone?
            self.audio_description = Some(audio_description.clone());
        }

        Ok(chunk)
    }

    /// Reads a chunk.
    ///
    /// Returns `None` if the end of the stream was reached.
    pub fn read_chunk(&mut self) -> Result<Option<Chunk>, CafReadError> {
        let maybe_chunk = Chunk::read(&mut self.rdr, self.audio_description.as_ref())?;

        // if we just read the Audio Description chunk, store it, since the Packet Table chunk needs it
        if let Some(Chunk::AudioDescription(audio_description)) = &maybe_chunk {
            // TODO: maybe it's possible to remove this clone?
            self.audio_description = Some(audio_description.clone());
        }

        Ok(maybe_chunk)
    }

    /// Reads a chunk's body as a Vec of bytes into a pre-allocated buffer.
    /// The length of the chunk's body is taken from the header.
    ///
    /// The output buffer is allowed to be longer than required.
    /// An error is returned if the buffer is too small.
    ///
    /// If this `ChunkReader` will be used to decode the Packet Table chunk (not raw) in the future,
    /// you should probably not use this method for the Audio Description chunk.
    /// This method does not store the Audio Description chunk internally in the `ChunkReader`,
    /// so decoding the Packet Table chunk later may fail.
    ///
    /// Reading a raw chunk an unspecified size (set to -1) is not implemented and will panic.
    pub fn read_chunk_raw_into_buffer<B: AsMut<[u8]>>(
        &mut self,
        header: &ChunkHeader,
        mut buf: B,
    ) -> Result<(), CafReadError> {
        if header.chunk_size == -1 {
            unimplemented!("unspecified chunk size is not implemented");
        }
        let chunk_size = header.chunk_size as usize;

        let buf = buf.as_mut();
        if buf.len() < chunk_size {
            return Err(CafReadError::BufferSize);
        }

        let buf_slice = &mut buf[0..chunk_size];
        self.rdr.read_exact(buf_slice)?;

        Ok(())
    }

    /// Reads a chunk's body and returns it as a Vec of bytes.
    /// The length of the chunk's body is taken from the header.
    ///
    /// If this `ChunkReader` will be used to decode the Packet Table chunk (not raw) in the future,
    /// you should probably not use this method for the Audio Description chunk.
    /// This method does not store the Audio Description chunk internally in the `ChunkReader`,
    /// so decoding the Packet Table chunk later may fail.
    ///
    /// This method is a convenience wrapper for [`read_chunk_raw_into_buffer`](ChunkReader::read_chunk_raw_into_buffer)
    /// that allocates a Vec as the output buffer.
    ///
    /// Reading a raw chunk an unspecified size (set to -1) is not implemented and will panic.
    pub fn read_chunk_raw(&mut self, header: &ChunkHeader) -> Result<Vec<u8>, CafReadError> {
        if header.chunk_size == -1 {
            unimplemented!("unspecified chunk size is not implemented");
        }
        let chunk_size = header.chunk_size as usize;

        let mut buf = vec![0; chunk_size];
        self.read_chunk_raw_into_buffer(header, &mut buf)?;

        Ok(buf)
    }
}

impl<T: Read + Seek> ChunkReader<T> {
    /// Seeks to the start of the next chunk header in the stream.
    ///
    /// This method is meant to be called immediately after a chunk's header has been read,
    /// with the internal reader's position at the start of the chunk's body.
    /// It then seeks to the start of the next chunk header.
    ///
    /// This method can be used to ignore chunks and skip reading them if they are
    /// not needed or if more information is needed before this chunk can be used.
    pub fn seek_to_next_chunk(&mut self, header: &ChunkHeader) -> Result<(), CafReadError> {
        if header.chunk_size == -1 {
            // seek to the end of the stream
            self.rdr.seek(SeekFrom::End(0))?;
        } else {
            // seek past the chunk body
            self.rdr.seek(SeekFrom::Current(header.chunk_size))?;
        }
        Ok(())
    }
}

// TODO: return the frames per packet somehow for variable frames per packet formats like vorbis

/// High-level utility for reading packets from a CAF stream.
///
/// The underlying reader must implement both [`Read`] and [`Seek`].
/// Since the Audio Data chunk may appear before the end of the file,
/// and we might not want to read the entire Audio Data chunk into memory,
/// we use [`Seek`] to return to the Audio Data chunk after reading other desired chunks.
pub struct PacketReader<T: Read + Seek> {
    rdr: Take<T>,
    packet_idx: usize,

    pub audio_description: AudioDescription,
    pub audio_data: AudioData,
    pub packet_table: Option<PacketTable>,

    /// User-requested extra chunks.
    pub extra_chunks: Vec<Chunk>,
}

impl<T: Read + Seek> PacketReader<T> {
    /// Constructs a new `PacketReader` from a given [`Read`].
    ///
    /// The `extra_chunk_types` argument can be used to decode additional chunk types as needed.
    /// The Audio Description, Audio Data, and Packet Table chunks are always decoded, and won't
    /// be available in `extra_chunks` even if requested in `extra_chunk_types`, but are available
    /// in the constructed `PacketReader` struct.
    pub fn new(rdr: T, extra_chunk_types: &[ChunkType]) -> Result<Self, CafReadError> {
        let ch_rdr = ChunkReader::new(rdr)?;
        PacketReader::from_chunk_reader(ch_rdr, extra_chunk_types)
    }

    /// Constructs a new [`PacketReader`] from a given [`ChunkReader`].
    /// The `ChunkReader` should be at the beginning of the stream immediately after the CAF file header.
    ///
    /// The `extra_chunk_types` argument can be used to decode additional chunk types as needed.
    /// The Audio Description, Audio Data, and Packet Table chunks are always decoded, and won't
    /// be available in `extra_chunks` even if requested in `extra_chunk_types`, but are available
    /// in the constructed `PacketReader` struct.
    pub fn from_chunk_reader(
        mut ch_rdr: ChunkReader<T>,
        extra_chunk_types: &[ChunkType],
    ) -> Result<Self, CafReadError> {
        // Read Audio Description chunk (required to be first)
        let first_chunk_header = ch_rdr
            .read_chunk_header()?
            .ok_or_else(|| read_invalid!("stream contains no chunks"))?;
        if first_chunk_header.chunk_type != ChunkType::AudioDescription {
            return Err(read_invalid!(
                "stream doesn't start with an Audio Description chunk"
            ));
        }
        let first_chunk = ch_rdr.read_chunk_body(&first_chunk_header)?;
        let Chunk::AudioDescription(audio_description) = first_chunk else {
            unreachable!()
        };

        // Read all chunks to find the packet table and any user-requested extra chunks
        let mut packet_table = None;
        let mut extra_chunks = Vec::new();
        while let Some(header) = ch_rdr.read_chunk_header()? {
            if header.chunk_type == ChunkType::PacketTable {
                let Chunk::PacketTable(chunk) = ch_rdr.read_chunk_body(&header)? else {
                    unreachable!()
                };
                packet_table = Some(chunk);
            } else if extra_chunk_types.contains(&header.chunk_type) {
                let chunk = ch_rdr.read_chunk_body(&header)?;
                extra_chunks.push(chunk);
            } else {
                // skip chunk data
                ch_rdr.seek_to_next_chunk(&header)?;
            }
        }

        let packet_table_required =
            audio_description.bytes_per_packet == 0 || audio_description.frames_per_packet == 0;

        if packet_table_required && packet_table.is_none() {
            return Err(read_invalid!("packet table is required but was not found"));
        }

        // Seek back to the start and skip the CAF header
        ch_rdr
            .get_mut()
            .seek(SeekFrom::Start(CAF_HEADER_MAGIC_LEN))?;

        // Read chunks again to find the Audio Data chunk
        let mut audio_data = None;
        while let Some(header) = ch_rdr.read_chunk_header()? {
            if header.chunk_type == ChunkType::AudioData {
                // read the edit count from the Audio Data chunk without consuming the data
                let chunk = AudioData::read_until_data(ch_rdr.get_mut(), header.chunk_size)?;
                audio_data = Some(chunk);
                break;
            } else {
                // skip chunk data
                ch_rdr.seek_to_next_chunk(&header)?;
            }
        }

        let Some(audio_data) = audio_data else {
            return Err(read_invalid!("audio data chunk was not found"));
        };

        // The inner reader should now be at the start of the audio data.
        // We don't need chunk reading functionality anymore, so take the inner reader.
        // We're also using io::Take to prevent reading past the end of the audio data into the next chunk.
        let rdr = ch_rdr
            .into_inner()
            .take(audio_data.data_len.unwrap_or(u64::MAX));

        Ok(Self {
            rdr,

            audio_description,
            audio_data,
            packet_table,
            extra_chunks,

            packet_idx: 0,
        })
    }

    /// Returns the wrapped reader, consuming the [`PacketReader`].
    pub fn into_inner(self) -> Take<T> {
        self.rdr
    }
    /// Returns a reference to the wrapped reader.
    pub fn get_ref(&self) -> &Take<T> {
        &self.rdr
    }
    /// Returns a mutable reference to the wrapped reader.
    pub fn get_mut(&mut self) -> &mut Take<T> {
        &mut self.rdr
    }

    /// Returns whether the audio data has variable bytes per packet.
    pub fn has_variable_packet_bytes(&self) -> bool {
        self.audio_description.bytes_per_packet == 0
    }

    /// Returns whether the audio data has variable frames per packet.
    pub fn has_variable_packet_frames(&self) -> bool {
        self.audio_description.frames_per_packet == 0
    }

    /// Returns the size of the next packet in bytes.
    ///
    /// This can be useful if you want to allocate the packet output buffer yourself.
    ///
    /// For variable length packets, returns `None` if all packets have been read according to the packet table.
    /// If this stream is not using a packet table, this method always returns `Some` with the byte size of a packet.
    /// `next_packet` will still return `None` when called at the end of the stream.
    pub fn next_packet_size(&self) -> Option<usize> {
        // packets are fixed length, fixed frames, so we can't check the packet table for EOF.
        // return the packet size from the Audio Description
        if !self.has_variable_packet_bytes() && !self.has_variable_packet_frames() {
            return Some(self.audio_description.bytes_per_packet as usize);
        }

        // packets are either variable length or variable frames, so the stream should have a packet table.
        // when the packet reader was constructed we already checked for this
        let packet_table = self
            .packet_table
            .as_ref()
            .expect("should have packet table");

        // packet table ran out of entries
        if self.next_packet_idx() >= packet_table.num_packets as usize {
            return None;
        }

        let bytes_per_packet = if self.has_variable_packet_bytes() {
            packet_table
                .bytes_per_packet
                .get(self.packet_idx)
                .copied()
                .expect("should be in bounds") as usize
        } else {
            self.audio_description.bytes_per_packet as usize
        };

        Some(bytes_per_packet)
    }

    /// Returns the index of the next packet.
    pub fn next_packet_idx(&self) -> usize {
        self.packet_idx
    }

    /// Reads a packet from the audio data into a pre-allocated buffer
    /// and returns the size of the packet in bytes.
    ///
    /// Returns `None` if the end of the stream was reached.
    ///
    /// The output buffer is allowed to be longer than required.
    /// An error is returned if the buffer is too small.
    pub fn read_packet_into_buffer<B: AsMut<[u8]>>(
        &mut self,
        mut buf: B,
    ) -> Result<Option<usize>, CafReadError> {
        let next_packet_size = match self.next_packet_size() {
            Some(v) => v,
            None => return Ok(None),
        };

        let buf = buf.as_mut();
        if buf.len() < next_packet_size {
            return Err(CafReadError::BufferSize);
        }

        let buf_slice = &mut buf[0..next_packet_size];
        match self.rdr.read_exact(buf_slice) {
            Ok(_) => {}
            Err(e) => {
                // reading failed
                return match e.kind() {
                    // if the error was due to EOF, return None to indicate the end of the stream.
                    //
                    // in some cases this might be a format error, for example if the packet table
                    // instructs us to keep reading past the end of the audio data chunk.
                    // we don't currently check for that though
                    std::io::ErrorKind::UnexpectedEof => Ok(None),

                    // otherwise return an error
                    _ => Err(e.into()),
                };
            }
        }

        self.packet_idx += 1;

        Ok(Some(next_packet_size))
    }

    /// Reads a packet from the audio data.
    ///
    /// Returns `None` if the end of the stream was reached.
    ///
    /// This method is a convenience wrapper for [`read_packet_into_buffer`](PacketReader::read_packet_into_buffer)
    /// that allocates a Vec as the output buffer.
    pub fn read_packet(&mut self) -> Result<Option<Vec<u8>>, CafReadError> {
        let next_packet_size = match self.next_packet_size() {
            Some(v) => v,
            None => return Ok(None),
        };

        let mut buf = vec![0; next_packet_size];

        let res = self.read_packet_into_buffer(&mut buf)?;
        // return None if read_packet_into_buffer returned None
        if res.is_none() {
            return Ok(None);
        }

        Ok(Some(buf))
    }

    /// Returns the number of packets in the stream if known.
    pub fn num_packets(&self) -> Option<usize> {
        if self.has_variable_packet_bytes() || self.has_variable_packet_frames() {
            let Some(packet_table) = self.packet_table.as_ref() else {
                // we should have a packet table and should have checked earlier,
                // but return None to be safe
                return None;
            };

            // return packet count from packet table
            Some(packet_table.num_packets as usize)
        } else {
            // constant bytes per packet

            // if AudioData.data_len is None, return None
            // otherwise return data_len / bytes per packet
            self.audio_data
                .data_len
                .map(|len| (len / self.audio_description.bytes_per_packet as u64) as usize)
        }
    }

    /// Seeks to start of the packet at the given index.
    pub fn seek_to_packet(&mut self, packet_idx: usize) -> Result<(), CafReadError> {
        let current_idx = self.packet_idx;

        // check if already at packet
        if current_idx == packet_idx {
            return Ok(());
        }

        let min_index = current_idx.min(packet_idx);
        let max_index = current_idx.max(packet_idx);

        let mut offset = 0;
        if self.audio_description.bytes_per_packet == 0 {
            // variable bytes per packet
            let packet_table = self
                .packet_table
                .as_ref()
                .ok_or(read_invalid!("should have packet table"))?;

            // sum packet lengths between current and target indices
            for i in min_index..max_index {
                offset += packet_table.bytes_per_packet.get(i).copied().unwrap_or(0) as i64;
            }
        } else {
            offset +=
                (max_index - min_index) as i64 * (self.audio_description.bytes_per_packet as i64);
        }

        if current_idx < packet_idx {
            // seek forward
            self.rdr.get_mut().seek(SeekFrom::Current(offset))?;
        } else {
            // seek backward
            self.rdr.get_mut().seek(SeekFrom::Current(-offset))?;
        }

        // update internal packet index
        self.packet_idx = packet_idx;

        Ok(())
    }
}
