// CAF container decoder written in Rust
//
// Copyright (c) 2017 est31 <MTest31@outlook.com>
// and contributors. All rights reserved.
// Licensed under MIT license, or Apache 2 license,
// at your option. Please see the LICENSE file
// attached to this source distribution for details.

use crate::reading::CafReadError;
use crate::reading::{invalid_chunk_size, invalid_data};
use crate::ChunkType;
use crate::FormatType;
use byteorder::{BigEndian as Be, ReadBytesExt};
use std::collections::HashMap;
use std::io::{BufRead, Cursor, Read};

// ReaD with big endian order and Try
macro_rules! rdt {
    ($rdr:ident, $func:ident) => {
        $rdr.$func::<Be>()?
    };
}

/// A decoded chunk header.
#[derive(Debug, Clone)]
pub struct ChunkHeader {
    pub chunk_type: ChunkType,

    /// The size of the chunk's body (without the head) in bytes.
    ///
    /// -1 is a special value and means the chunk comes last and ends at the EOF.
    /// This is only allowed for the Audio Data chunk.
    pub chunk_size: i64,
}

impl ChunkHeader {
    /// Reads a chunk header from a reader.
    ///
    /// Returns `None` if the end of the stream was reached.
    pub fn read<T: Read>(mut rdr: T) -> Result<Option<Self>, CafReadError> {
        let chunk_type = match rdr.read_u32::<Be>() {
            Ok(chunk_type) => ChunkType::from(chunk_type),
            Err(e) => {
                // reading failed
                return match e.kind() {
                    // if the error was due to EOF, return None
                    std::io::ErrorKind::UnexpectedEof => Ok(None),
                    // otherwise return an error
                    _ => Err(e.into()),
                };
            }
        };

        let chunk_size = rdt!(rdr, read_i64);
        if chunk_size < 0 && chunk_size != -1 {
            return Err(invalid_data!("chunk size is less than -1"));
        }

        Ok(Some(Self {
            chunk_type,
            chunk_size,
        }))
    }
}

/// A decoded chunk.
///
/// This enum lists the chunk types that this library can parse.
#[derive(Debug, Clone)]
pub enum Chunk {
    AudioDescription(AudioDescription),
    AudioData(AudioData),
    PacketTable(PacketTable),
    ChannelLayout(ChannelLayout),
    MagicCookie(Vec<u8>),
    Information(HashMap<String, String>),
}

impl Chunk {
    /// Reads a chunk from a reader.
    ///
    /// Returns `None` if the end of the stream was reached.
    ///
    /// Some chunks require information from the Audio Description chunk in order to be read properly
    /// (notably, the Packet Table chunk). The Audio Description chunk is required to be first in the file,
    /// so it should always be passed back to this function once it's been read.
    pub fn read<T: Read>(
        mut rdr: T,
        audio_description: Option<&AudioDescription>,
    ) -> Result<Option<Self>, CafReadError> {
        if let Some(header) = ChunkHeader::read(&mut rdr)? {
            Self::read_body(rdr, &header, audio_description).map(Some)
        } else {
            Ok(None)
        }
    }

    /// Reads the body of a chunk from a reader using the chunk's header.
    ///
    /// Some chunks require information from the Audio Description chunk in order to be read properly
    /// (notably, the Packet Table chunk). The Audio Description chunk is required to be first in the file,
    /// so it should always be passed back to this function once it's been read.
    pub fn read_body<T: Read>(
        mut rdr: T,
        header: &ChunkHeader,
        audio_description: Option<&AudioDescription>,
    ) -> Result<Self, CafReadError> {
        Ok(match header.chunk_type {
            ChunkType::AudioDescription => {
                Chunk::AudioDescription(AudioDescription::read(rdr, header.chunk_size)?)
            }

            ChunkType::AudioData => Chunk::AudioData(AudioData::read(rdr, header.chunk_size)?),

            ChunkType::PacketTable => {
                let audio_desc = audio_description.ok_or(invalid_data!(
                    "encountered Packet Table chunk but Audio Description was not provided"
                ))?;
                let variable_bytes_per_packet = audio_desc.bytes_per_packet == 0;
                let variable_frames_per_packet = audio_desc.frames_per_packet == 0;
                Chunk::PacketTable(PacketTable::read(
                    rdr,
                    header.chunk_size,
                    variable_bytes_per_packet,
                    variable_frames_per_packet,
                )?)
            }

            ChunkType::ChannelLayout => {
                Chunk::ChannelLayout(ChannelLayout::read(rdr, header.chunk_size)?)
            }

            ChunkType::MagicCookie => {
                if header.chunk_size == -1 {
                    return Err(invalid_chunk_size!("Magic Cookie"));
                }
                let chunk_size = header.chunk_size as usize;

                let mut buf = vec![0; chunk_size];
                rdr.read_exact(&mut buf)?;

                Chunk::MagicCookie(buf)
            }

            ChunkType::Information => {
                if header.chunk_size == -1 {
                    return Err(invalid_chunk_size!("Magic Cookie"));
                }
                let chunk_size = header.chunk_size as usize;

                // read the whole chunk body to a buffer first so we can use read_until
                // this also ensures that we consume the entire chunk body, not just the used space
                let mut buf = vec![0; chunk_size];
                rdr.read_exact(&mut buf)?;
                let mut buf_rdr = Cursor::new(buf);

                let num_entries = rdt!(buf_rdr, read_u32);

                let mut entries = Vec::with_capacity(num_entries as usize);
                for _ in 0..num_entries {
                    // read null-terminated utf8 strings
                    let mut key = Vec::new();
                    buf_rdr.read_until(0, &mut key)?;
                    let mut val = Vec::new();
                    buf_rdr.read_until(0, &mut val)?;

                    // remove the trailing \0's
                    key.pop();
                    val.pop();

                    entries.push((String::from_utf8(key)?, String::from_utf8(val)?));
                }

                let map = HashMap::from_iter(entries);
                Chunk::Information(map)
            }

            _ => return Err(CafReadError::UnsupportedChunkType(header.chunk_type)),
        })
    }

    /// Returns the [`ChunkType`] of this chunk.
    pub fn chunk_type(&self) -> ChunkType {
        use ChunkType::*;
        match self {
            Chunk::AudioDescription(..) => AudioDescription,
            Chunk::AudioData(..) => AudioData,
            Chunk::PacketTable(..) => PacketTable,
            Chunk::ChannelLayout(..) => ChannelLayout,
            Chunk::MagicCookie(..) => MagicCookie,
            Chunk::Information(..) => Information,
        }
    }
}

/// An Audio Description chunk.
#[derive(Debug, Clone)]
pub struct AudioDescription {
    pub sample_rate: f64,
    pub format_id: FormatType,
    pub format_flags: u32,
    pub bytes_per_packet: u32,
    pub frames_per_packet: u32,
    pub channels_per_frame: u32,
    pub bits_per_channel: u32,
}

impl AudioDescription {
    /// Reads an Audio Description chunk from a reader.
    pub fn read<T: Read>(mut rdr: T, chunk_size: i64) -> Result<Self, CafReadError> {
        if chunk_size != 32 {
            return Err(invalid_chunk_size!("Audio Description"));
        }

        let sample_rate = rdt!(rdr, read_f64);
        if sample_rate == 0.0 {
            return Err(invalid_data!(
                "audio description sample rate must be nonzero"
            ));
        }

        let format_id = FormatType::from(rdt!(rdr, read_u32));
        let format_flags = rdt!(rdr, read_u32);

        let bytes_per_packet = rdt!(rdr, read_u32);
        let frames_per_packet = rdt!(rdr, read_u32);

        let channels_per_frame = rdt!(rdr, read_u32);
        if channels_per_frame == 0 {
            return Err(invalid_data!(
                "audio description channels per frame must be nonzero"
            ));
        }

        let bits_per_channel = rdt!(rdr, read_u32);

        Ok(Self {
            sample_rate,
            format_id,
            format_flags,
            bytes_per_packet,
            frames_per_packet,
            channels_per_frame,
            bits_per_channel,
        })
    }
}

/// An Audio Data chunk.
///
/// Reading this chunk doesn't read the full audio data in memory.
#[derive(Debug, Clone)]
pub struct AudioData {
    pub edit_count: u32,
    /// The length of the audio data, or None if the chunk size in the header was -1.
    pub data_len: Option<u64>,
}

impl AudioData {
    /// Reads an Audio Data chunk from a reader, consuming the audio data.
    /// The full audio data is not read into memory when using this method.
    pub fn read<T: Read>(mut rdr: T, chunk_size: i64) -> Result<Self, CafReadError> {
        let chunk = Self::read_until_data(&mut rdr, chunk_size)?;

        if let Some(data_len) = chunk.data_len {
            // consume `data_len` bytes
            std::io::copy(&mut rdr.take(data_len), &mut std::io::sink())?;
        }

        Ok(chunk)
    }

    /// Reads an Audio Data chunk from a reader without consuming the audio data.
    /// If this method is used, `data_len` bytes of audio data must be read before reaching the next chunk.
    pub fn read_until_data<T: Read>(mut rdr: T, chunk_size: i64) -> Result<Self, CafReadError> {
        let edit_count_size = std::mem::size_of::<u32>() as i64;

        if chunk_size != -1 && chunk_size < edit_count_size {
            return Err(invalid_chunk_size!("Audio Data"));
        }

        let edit_count = rdt!(rdr, read_u32);

        let data_len = if chunk_size == -1 {
            None
        } else {
            Some((chunk_size - edit_count_size) as u64)
        };

        Ok(Self {
            edit_count,
            data_len,
        })
    }
}

/// A Packet Table chunk.
#[derive(Debug, Clone)]
pub struct PacketTable {
    pub num_packets: i64,
    pub valid_frames: i64,
    pub priming_frames: i32,
    pub remainder_frames: i32,
    pub bytes_per_packet: Vec<u64>,
    pub frames_per_packet: Vec<u64>,
}

impl PacketTable {
    /// Reads a Packet Table chunk from a reader.
    ///
    /// Some information from the Audio Description chunk must be passed in to correctly read the packet data.
    /// The Audio Description chunk is required to appear first in the file, so that information should always be available.
    pub fn read<T: Read>(
        mut rdr: T,
        chunk_size: i64,
        variable_bytes_per_packet: bool,
        variable_frames_per_packet: bool,
    ) -> Result<Self, CafReadError> {
        if chunk_size < 24 {
            return Err(invalid_chunk_size!("Packet Table"));
        }

        let num_packets = rdt!(rdr, read_i64);
        if num_packets < 0 {
            return Err(invalid_data!("packet table number of packets is invalid"));
        }
        if !variable_bytes_per_packet && !variable_frames_per_packet && num_packets > 0 {
            return Err(invalid_data!(
                "packet size is constant but packet table contains packet lengths"
            ));
        }

        let valid_frames = rdt!(rdr, read_i64);
        if valid_frames < 0 {
            return Err(invalid_data!(
                "packet table number of valid frames is invalid"
            ));
        }

        let priming_frames = rdt!(rdr, read_i32);
        let remainder_frames = rdt!(rdr, read_i32);

        let mut bytes_per_packet = Vec::new();
        let mut frames_per_packet = Vec::new();

        if variable_bytes_per_packet && variable_frames_per_packet {
            bytes_per_packet.reserve_exact(num_packets as usize);
            frames_per_packet.reserve_exact(num_packets as usize);
            for _ in 0..num_packets {
                let bytes = read_vlq(&mut rdr)?;
                let frames = read_vlq(&mut rdr)?;
                bytes_per_packet.push(bytes);
                frames_per_packet.push(frames);
            }
        } else if variable_bytes_per_packet {
            bytes_per_packet.reserve_exact(num_packets as usize);
            for _ in 0..num_packets {
                let bytes = read_vlq(&mut rdr)?;
                bytes_per_packet.push(bytes);
            }
        } else if variable_frames_per_packet {
            frames_per_packet.reserve_exact(num_packets as usize);
            for _ in 0..num_packets {
                let frames = read_vlq(&mut rdr)?;
                frames_per_packet.push(frames);
            }
        } else {
            // we checked earlier that num_packets is 0 for constant size packets,
            // so there shouldn't be any more data to read before the next chunk header starts
        }

        Ok(Self {
            num_packets,
            valid_frames,
            priming_frames,
            remainder_frames,
            bytes_per_packet,
            frames_per_packet,
        })
    }
}

/// A channel description found in a Channel Layout chunk.
#[derive(Debug, Clone)]
pub struct ChannelDescription {
    pub channel_label: u32,
    pub channel_flags: u32,
    pub coordinates: (f32, f32, f32),
}

impl ChannelDescription {
    /// Reads a channel description from a reader.
    pub fn read<T: Read>(mut rdr: T) -> Result<Self, CafReadError> {
        let channel_label = rdt!(rdr, read_u32);
        let channel_flags = rdt!(rdr, read_u32);
        let coordinates = (
            rdt!(rdr, read_f32),
            rdt!(rdr, read_f32),
            rdt!(rdr, read_f32),
        );
        Ok(Self {
            channel_label,
            channel_flags,
            coordinates,
        })
    }
}

/// A Channel Layout chunk.
#[derive(Debug, Clone)]
pub struct ChannelLayout {
    // TODO enrich this one and the one below with some meaning
    // e.g. we'll maybe need some other representation, like an enum?
    pub channel_layout_tag: u32,
    pub channel_bitmap: u32,
    pub channel_descriptions: Vec<ChannelDescription>,
}

impl ChannelLayout {
    /// Reads a Channel Layout chunk from a reader.
    pub fn read<T: Read>(mut rdr: T, chunk_size: i64) -> Result<Self, CafReadError> {
        if chunk_size < 12 {
            return Err(invalid_chunk_size!("Channel Layout"));
        }

        let channel_layout_tag = rdt!(rdr, read_u32);
        let channel_bitmap = rdt!(rdr, read_u32);

        let num_channel_descriptions = rdt!(rdr, read_u32);
        let mut channel_descriptions = Vec::with_capacity(num_channel_descriptions as usize);
        for _ in 0..num_channel_descriptions {
            let desc = ChannelDescription::read(&mut rdr)?;
            channel_descriptions.push(desc);
        }

        Ok(Self {
            channel_layout_tag,
            channel_bitmap,
            channel_descriptions,
        })
    }
}

/// Reads a variable-length quantity from a [`Read`].
fn read_vlq<T: Read>(mut rdr: T) -> Result<u64, CafReadError> {
    let mut res = 0;
    let mut buf = [0; 1];
    for _ in 0..9 {
        rdr.read_exact(&mut buf)?;
        let byte = buf[0];
        res |= (byte & 127) as u64;
        if byte & 128 == 0 {
            return Ok(res);
        }
        res <<= 7;
    }

    Err(invalid_data!("unterminated variable-length integer"))
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    fn test_vlq(bytes: &[u8], expected: u64) {
        let mut cursor = Cursor::new(Vec::from(bytes));
        let res = super::read_vlq(&mut cursor);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn read_vlq() {
        // examples from the spec
        test_vlq(&[0x01], 1);
        test_vlq(&[0x11], 17);
        test_vlq(&[0x7f], 127);
        test_vlq(&[0x81, 0x00], 128);
        test_vlq(&[0x81, 0x02], 130);
        test_vlq(&[0x82, 0x01], 257);
        test_vlq(&[0xff, 0x7f], 16383);
        test_vlq(&[0x81, 0x80, 0x00], 16384);
    }

    #[test]
    fn unterminated_vlq() {
        let mut cursor = Cursor::new(&[0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]);
        assert!(super::read_vlq(&mut cursor).is_err());
    }
}
