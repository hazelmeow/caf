use crate::chunks::{AudioDescription, Chunk, ChunkHeader, PacketTable};
use crate::{ChunkType, CAF_HEADER_MAGIC};
use byteorder::{BigEndian as Be, WriteBytesExt};
use std::borrow::Cow;
use std::error::Error;
use std::fmt::Display;
use std::io::{Error as IoError, Write};

/// An error related to writing.
#[derive(Debug)]
pub enum CafWriteError {
    /// Wrapped I/O error.
    Io(IoError),
    /// Audio data size is not known.
    AudioDataSize,
    /// Invalid data.
    InvalidData(Cow<'static, str>),
}

impl CafWriteError {
    fn description_str(&self) -> &str {
        match &self {
            CafWriteError::Io(_) => "I/O error",
            CafWriteError::AudioDataSize => "Audio data size is not known",
            CafWriteError::InvalidData(s) => s,
        }
    }
}

impl Error for CafWriteError {
    fn description(&self) -> &str {
        self.description_str()
    }

    fn cause(&self) -> Option<&dyn Error> {
        match self {
            CafWriteError::Io(ref err) => Some(err as &dyn Error),
            _ => None,
        }
    }
}

impl Display for CafWriteError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Self::description_str(self))
    }
}

impl From<IoError> for CafWriteError {
    fn from(err: IoError) -> CafWriteError {
        CafWriteError::Io(err)
    }
}

macro_rules! write_invalid {
    ($text:expr) => {
        CafWriteError::InvalidData(std::borrow::Cow::Borrowed(concat!("Invalid data: ", $text)))
    };
}

pub(crate) use write_invalid;

/// High-level utility for writing packets to a CAF stream.
///
/// Packets can be added with the [`add_packet`](PacketWriter::add_packet) method.
/// After all packets have been added, the [`write_audio_data`](PacketWriter::write_audio_data) method
/// should be called to write the Audio Data and Packet Table chunks to the stream.
/// The `set_edit_count`, `set_priming_frames`, and `set_remainder_frames` methods
/// can be used before calling `write_audio_data` to set the corresponding values
/// written in the Audio Data and Packet Table chunks.
///
/// An [`AudioDescription`] chunk must be provided to construct a [`PacketWriter`].
/// Other chunks can be written to the stream with the [`write_chunk`](PacketWriter::write_chunk) method.
///
/// This struct internally buffers all added packets in memory and writes them all later
/// when [`write_audio_data`](PacketWriter::write_audio_data) is called.
/// This helps with writing the Packet Table chunk and Audio Data chunk header.
/// Writing these chunks could potentially be done more efficiently if the number of packets
/// is known ahead of time, or if the stream does not require a packet table.
pub struct PacketWriter<T: Write> {
    wtr: T,

    bytes_per_packet: u32,
    frames_per_packet: u32,

    num_packets: i64,
    data: Vec<u8>,
    bytes_per_packet_list: Vec<u64>,
    frames_per_packet_list: Vec<u64>,

    edit_count: u32,
    priming_frames: i32,
    remainder_frames: i32,
}

impl<T: Write> PacketWriter<T> {
    /// Constructs a new [`PacketWriter`] from a given [`Write`].
    pub fn new(mut wtr: T, audio_description: &AudioDescription) -> Result<Self, CafWriteError> {
        // write the CAF header
        wtr.write_all(&CAF_HEADER_MAGIC)?;

        let bytes_per_packet = audio_description.bytes_per_packet;
        let frames_per_packet = audio_description.frames_per_packet;

        // write the Audio Description chunk
        let audio_desc_header = ChunkHeader {
            chunk_type: ChunkType::AudioDescription,
            chunk_size: audio_description.write_body_size(),
        };
        audio_desc_header.write(&mut wtr)?;
        audio_description.write_body(&mut wtr)?;

        Ok(Self {
            wtr,

            bytes_per_packet,
            frames_per_packet,

            num_packets: 0,
            data: Vec::new(),
            bytes_per_packet_list: Vec::new(),
            frames_per_packet_list: Vec::new(),

            edit_count: 0,
            priming_frames: 0,
            remainder_frames: 0,
        })
    }

    /// Returns the wrapped writer, consuming the [`PacketWriter`].
    pub fn into_inner(self) -> T {
        self.wtr
    }
    /// Returns a reference to the wrapped writer.
    pub fn get_ref(&self) -> &T {
        &self.wtr
    }
    /// Returns a mutable reference to the wrapped writer.
    pub fn get_mut(&mut self) -> &mut T {
        &mut self.wtr
    }

    /// Writes an arbitrary chunk to the stream.
    pub fn write_chunk(&mut self, chunk: &Chunk) -> Result<(), CafWriteError> {
        chunk.write(&mut self.wtr)?;
        Ok(())
    }

    /// Adds a packet to be written when [`write_audio_data`](PacketWriter::write_audio_data) is called.
    ///
    /// If this stream uses variable frames per packet (set to 0 in the Audio Description chunk),
    /// `add_packet` must be called with a value for `packet_frames`.
    pub fn add_packet(
        &mut self,
        packet: &[u8],
        packet_frames: Option<u64>,
    ) -> Result<(), CafWriteError> {
        // check packet length
        if self.bytes_per_packet != 0 && packet.len() as u32 != self.bytes_per_packet {
            // fixed bytes per packet but packet length is wrong
            return Err(write_invalid!(
                "packet length does not match bytes_per_packet"
            ));
        }

        // check variable frames per packet
        if self.frames_per_packet == 0 && packet_frames.is_none() {
            return Err(write_invalid!(
                "variable frames per packet but packet_frames is None"
            ))?;
        }

        // store
        self.num_packets += 1;
        self.data.extend(packet);

        if self.bytes_per_packet == 0 {
            // variable bytes per packet, store packet length
            self.bytes_per_packet_list.push(packet.len() as u64);
        }

        if self.frames_per_packet == 0 {
            // variable frames per packet, store packet frames
            self.frames_per_packet_list.push(packet_frames.unwrap());
        }

        Ok(())
    }

    /// Writes the Audio Data chunk to the stream.
    /// Also writes a corresponding Packet Table chunk if required.
    ///
    /// After calling this method, no more packets should be written to the [`PacketWriter`].
    pub fn write_audio_data(&mut self) -> Result<(), CafWriteError> {
        let valid_frames = if self.frames_per_packet == 0 {
            // variable frames per packet, sum frames per packet
            self.frames_per_packet_list.iter().map(|x| *x as i64).sum()
        } else {
            self.frames_per_packet as i64 * self.num_packets
        };

        let bytes_per_packet_list = std::mem::take(&mut self.bytes_per_packet_list);
        let frames_per_packet_list = std::mem::take(&mut self.frames_per_packet_list);

        // only write num_packets if we have variable bytes per packet or variable frames per packet
        let num_packets = if self.bytes_per_packet == 0 || self.frames_per_packet == 0 {
            self.num_packets
        } else {
            0
        };

        let packet_table = PacketTable {
            num_packets,
            valid_frames,
            priming_frames: self.priming_frames,
            remainder_frames: self.remainder_frames,
            bytes_per_packet: bytes_per_packet_list,
            frames_per_packet: frames_per_packet_list,
        };

        self.write_chunk(&Chunk::PacketTable(packet_table))?;

        // audio data header
        let data_len = self.data.len() as i64;
        let audio_data_header = ChunkHeader {
            chunk_type: ChunkType::AudioData,
            chunk_size: 4 + data_len,
        };
        audio_data_header.write(&mut self.wtr)?;

        // audio data body
        self.wtr.write_u32::<Be>(self.edit_count)?;
        self.wtr.write_all(&self.data)?;

        Ok(())
    }

    /// Sets the value of `edit_count` in the Audio Data chunk
    /// written by [`write_audio_data`](PacketWriter::write_audio_data).
    pub fn set_edit_count(&mut self, val: u32) -> &mut Self {
        self.edit_count = val;
        self
    }

    /// Sets the value of `priming_frames` in the Packet Table chunk
    /// written by [`write_audio_data`](PacketWriter::write_audio_data).
    pub fn set_priming_frames(&mut self, val: i32) -> &mut Self {
        self.priming_frames = val;
        self
    }

    /// Sets the value of `remainder_frames` in the Packet Table chunk
    /// written by [`write_audio_data`](PacketWriter::write_audio_data).
    pub fn set_remainder_frames(&mut self, val: i32) -> &mut Self {
        self.remainder_frames = val;
        self
    }
}
