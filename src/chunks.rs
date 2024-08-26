// CAF container decoder written in Rust
//
// Copyright (c) 2017 est31 <MTest31@outlook.com>
// and contributors. All rights reserved.
// Licensed under MIT license, or Apache 2 license,
// at your option. Please see the LICENSE file
// attached to this source distribution for details.

use crate::reading::{invalid_chunk_size, read_invalid, CafReadError};
use crate::writing::{write_invalid, CafWriteError};
use crate::ChunkType;
use crate::FormatType;
use byteorder::{BigEndian as Be, ReadBytesExt, WriteBytesExt};
use std::collections::HashMap;
use std::io::{BufRead, Cursor, Read, Write};

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

        let chunk_size = rdr.read_i64::<Be>()?;
        if chunk_size < 0 && chunk_size != -1 {
            return Err(read_invalid!("chunk size is less than -1"));
        }

        Ok(Some(Self {
            chunk_type,
            chunk_size,
        }))
    }

    /// Writes this chunk header to a writer.
    pub fn write<T: Write>(&self, mut wtr: T) -> Result<(), CafWriteError> {
        wtr.write_u32::<Be>(self.chunk_type.as_u32())?;
        wtr.write_i64::<Be>(self.chunk_size)?;
        Ok(())
    }
}

/// A decoded chunk.
///
/// This enum lists the chunk types that this crate can read and write.
#[derive(Debug, Clone)]
pub enum Chunk {
    AudioDescription(AudioDescription),
    AudioData(AudioData),
    PacketTable(PacketTable),
    ChannelLayout(ChannelLayout),
    MagicCookie(MagicCookie),
    Information(Information),
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
        rdr: T,
        header: &ChunkHeader,
        audio_description: Option<&AudioDescription>,
    ) -> Result<Self, CafReadError> {
        Ok(match header.chunk_type {
            ChunkType::AudioDescription => {
                Chunk::AudioDescription(AudioDescription::read(rdr, header.chunk_size)?)
            }

            ChunkType::AudioData => Chunk::AudioData(AudioData::read(rdr, header.chunk_size)?),

            ChunkType::PacketTable => {
                let audio_desc = audio_description.ok_or(read_invalid!(
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
                Chunk::MagicCookie(MagicCookie::read(rdr, header.chunk_size)?)
            }

            ChunkType::Information => {
                Chunk::Information(Information::read(rdr, header.chunk_size)?)
            }

            _ => return Err(CafReadError::UnsupportedChunkType(header.chunk_type)),
        })
    }

    /// Writes this chunk's header to a writer.
    ///
    /// Always returns an error if called on a [`Chunk::AudioData`].
    /// Since the [`AudioData`] struct doesn't hold the full audio data in memory,
    /// the size of the chunk cannot be known which is required to write the chunk header.
    fn write_header<T: Write>(&self, wtr: T) -> Result<(), CafWriteError> {
        let chunk_type = self.chunk_type();

        // can't known chunk size for AudioData
        if chunk_type == ChunkType::AudioData {
            return Err(CafWriteError::AudioDataSize);
        }

        let header = ChunkHeader {
            chunk_type,
            chunk_size: self.write_body_size(),
        };
        header.write(wtr)?;
        Ok(())
    }

    /// Returns the number of bytes that will be written by [`write_body`](Chunk::write_body).
    ///
    /// This value will not be fully correct for [`Chunk::AudioData`] since the [`AudioData`] struct
    /// does not hold the full audio data in memory. It will still return the number of bytes
    /// written by `write_body` but `write_body` will not write the full chunk body.
    pub fn write_body_size(&self) -> i64 {
        match self {
            Chunk::AudioDescription(c) => c.write_body_size(),
            Chunk::AudioData(c) => c.write_body_size(),
            Chunk::PacketTable(c) => c.write_body_size(),
            Chunk::ChannelLayout(c) => c.write_body_size(),
            Chunk::MagicCookie(c) => c.write_body_size(),
            Chunk::Information(c) => c.write_body_size(),
        }
    }

    /// Writes this chunk's body to a writer.
    ///
    /// When called on a [`Chunk::AudioData`], this method will not write the full body of the chunk
    /// since the [`AudioData`] struct does not hold the full audio data in memory.
    /// It will still write the edit count contained in the struct.
    /// The audio data should be written immediately afterwards.
    pub fn write_body<T: Write>(&self, wtr: T) -> Result<(), CafWriteError> {
        match self {
            Chunk::AudioDescription(c) => c.write_body(wtr),
            Chunk::AudioData(c) => c.write_body(wtr),
            Chunk::PacketTable(c) => c.write_body(wtr),
            Chunk::ChannelLayout(c) => c.write_body(wtr),
            Chunk::MagicCookie(c) => c.write_body(wtr),
            Chunk::Information(c) => c.write_body(wtr),
        }
    }

    /// Writes this chunk to a writer.
    ///
    /// Always returns an error if called on a [`Chunk::AudioData`].
    /// Since the [`AudioData`] struct doesn't hold the full audio data in memory,
    /// the size of the chunk cannot be known which is required to write the chunk header.
    pub fn write<T: Write>(&self, mut wtr: T) -> Result<(), CafWriteError> {
        self.write_header(&mut wtr)?;
        self.write_body(&mut wtr)?;
        Ok(())
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

        let sample_rate = rdr.read_f64::<Be>()?;
        if sample_rate == 0.0 {
            return Err(read_invalid!(
                "audio description sample rate must be nonzero"
            ));
        }

        let format_id = FormatType::from(rdr.read_u32::<Be>()?);
        let format_flags = rdr.read_u32::<Be>()?;

        let bytes_per_packet = rdr.read_u32::<Be>()?;
        let frames_per_packet = rdr.read_u32::<Be>()?;

        let channels_per_frame = rdr.read_u32::<Be>()?;
        if channels_per_frame == 0 {
            return Err(read_invalid!(
                "audio description channels per frame must be nonzero"
            ));
        }

        let bits_per_channel = rdr.read_u32::<Be>()?;

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

    /// Returns the number of bytes that will be written by [`write_body`](AudioDescription::write_body).
    pub fn write_body_size(&self) -> i64 {
        32
    }

    /// Writes this chunk's body to a writer.
    pub fn write_body<T: Write>(&self, mut wtr: T) -> Result<(), CafWriteError> {
        wtr.write_f64::<Be>(self.sample_rate)?;
        wtr.write_u32::<Be>(self.format_id.as_u32())?;
        wtr.write_u32::<Be>(self.format_flags)?;
        wtr.write_u32::<Be>(self.bytes_per_packet)?;
        wtr.write_u32::<Be>(self.frames_per_packet)?;
        wtr.write_u32::<Be>(self.channels_per_frame)?;
        wtr.write_u32::<Be>(self.bits_per_channel)?;
        Ok(())
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

        let edit_count = rdr.read_u32::<Be>()?;

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

    /// Returns the number of bytes that will be written by [`write_body`](AudioDescription::write_body).
    ///
    /// Since this struct does not hold the full audio data in memory, `write_body` will not
    /// write the full body of the chunk. This method still returns the number of bytes written
    /// by `write_body` for the edit count.
    pub fn write_body_size(&self) -> i64 {
        4
    }

    /// Writes this chunk's body to a writer.
    ///
    /// Since this struct does not hold the full audio data in memory, this method will not
    /// write the full body of the chunk, only the edit count at the beginning of the chunk.
    /// The rest of the audio data should be written immediately afterwards.
    pub fn write_body<T: Write>(&self, mut wtr: T) -> Result<(), CafWriteError> {
        wtr.write_u32::<Be>(self.edit_count)?;
        Ok(())
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

        let num_packets = rdr.read_i64::<Be>()?;
        if num_packets < 0 {
            return Err(read_invalid!("packet table number of packets is invalid"));
        }
        if !variable_bytes_per_packet && !variable_frames_per_packet && num_packets > 0 {
            return Err(read_invalid!(
                "packet size is constant but packet table contains packet lengths"
            ));
        }

        let valid_frames = rdr.read_i64::<Be>()?;
        if valid_frames < 0 {
            return Err(read_invalid!(
                "packet table number of valid frames is invalid"
            ));
        }

        let priming_frames = rdr.read_i32::<Be>()?;
        let remainder_frames = rdr.read_i32::<Be>()?;

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

    /// Returns the number of bytes that will be written by [`write_body`](PacketTable::write_body).
    pub fn write_body_size(&self) -> i64 {
        let bytes_per_packet_len: i64 = self
            .bytes_per_packet
            .iter()
            .map(|v| vlq_size(*v) as i64)
            .sum();
        let frames_per_packet_len: i64 = self
            .frames_per_packet
            .iter()
            .map(|v| vlq_size(*v) as i64)
            .sum();

        24 + bytes_per_packet_len + frames_per_packet_len
    }

    /// Writes this chunk's body to a writer.
    pub fn write_body<T: Write>(&self, mut wtr: T) -> Result<(), CafWriteError> {
        wtr.write_i64::<Be>(self.num_packets)?;
        wtr.write_i64::<Be>(self.valid_frames)?;
        wtr.write_i32::<Be>(self.priming_frames)?;
        wtr.write_i32::<Be>(self.remainder_frames)?;

        // write packets
        if self.num_packets > 0 {
            // check lengths
            let has_bytes_per_packet = !self.bytes_per_packet.is_empty();
            if has_bytes_per_packet && self.bytes_per_packet.len() != self.num_packets as usize {
                return Err(write_invalid!(
                    "packet table bytes_per_packet len != num_packets"
                ));
            }
            let has_frames_per_packet = !self.frames_per_packet.is_empty();
            if has_frames_per_packet && self.frames_per_packet.len() != self.num_packets as usize {
                return Err(write_invalid!(
                    "packet table frames_per_packet len != num_packets"
                ));
            }

            for i in 0..self.num_packets as usize {
                if has_bytes_per_packet {
                    write_vlq(&mut wtr, self.bytes_per_packet[i])?;
                }
                if has_frames_per_packet {
                    write_vlq(&mut wtr, self.frames_per_packet[i])?;
                }
            }
        }

        Ok(())
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
        let channel_label = rdr.read_u32::<Be>()?;
        let channel_flags = rdr.read_u32::<Be>()?;
        let coordinates = (
            rdr.read_f32::<Be>()?,
            rdr.read_f32::<Be>()?,
            rdr.read_f32::<Be>()?,
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

        let channel_layout_tag = rdr.read_u32::<Be>()?;
        let channel_bitmap = rdr.read_u32::<Be>()?;

        let num_channel_descriptions = rdr.read_u32::<Be>()?;
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

    /// Returns the number of bytes that will be written by [`write_body`](ChannelLayout::write_body).
    pub fn write_body_size(&self) -> i64 {
        12 + self.channel_descriptions.len() as i64 * 20
    }

    /// Writes this chunk's body to a writer.
    pub fn write_body<T: Write>(&self, mut wtr: T) -> Result<(), CafWriteError> {
        wtr.write_u32::<Be>(self.channel_layout_tag)?;
        wtr.write_u32::<Be>(self.channel_bitmap)?;
        wtr.write_u32::<Be>(self.channel_descriptions.len() as u32)?;
        for channel in &self.channel_descriptions {
            wtr.write_u32::<Be>(channel.channel_label)?;
            wtr.write_u32::<Be>(channel.channel_flags)?;
            wtr.write_f32::<Be>(channel.coordinates.0)?;
            wtr.write_f32::<Be>(channel.coordinates.1)?;
            wtr.write_f32::<Be>(channel.coordinates.2)?;
        }
        Ok(())
    }
}

/// A Magic Cookie chunk.
#[derive(Debug, Clone)]
pub struct MagicCookie(pub Vec<u8>);

impl MagicCookie {
    /// Reads a Magic Cookie chunk from a reader.
    pub fn read<T: Read>(mut rdr: T, chunk_size: i64) -> Result<Self, CafReadError> {
        if chunk_size == -1 {
            return Err(invalid_chunk_size!("Magic Cookie"));
        }
        let chunk_size = chunk_size as usize;

        let mut buf = vec![0; chunk_size];
        rdr.read_exact(&mut buf)?;

        Ok(Self(buf))
    }

    /// Returns the number of bytes that will be written by [`write_body`](MagicCookie::write_body).
    pub fn write_body_size(&self) -> i64 {
        self.0.len() as i64
    }

    /// Writes this chunk's body to a writer.
    pub fn write_body<T: Write>(&self, mut wtr: T) -> Result<(), CafWriteError> {
        wtr.write_all(&self.0)?;
        Ok(())
    }
}

/// An Information chunk.
#[derive(Debug, Clone)]
pub struct Information(pub HashMap<String, String>);

impl Information {
    /// Reads an Information chunk from a reader.
    pub fn read<T: Read>(mut rdr: T, chunk_size: i64) -> Result<Self, CafReadError> {
        if chunk_size == -1 {
            return Err(invalid_chunk_size!("Magic Cookie"));
        }
        let chunk_size = chunk_size as usize;

        // read the whole chunk body to a buffer first so we can use read_until
        // this also ensures that we consume the entire chunk body, not just the used space
        let mut buf = vec![0; chunk_size];
        rdr.read_exact(&mut buf)?;
        let mut buf_rdr = Cursor::new(buf);

        let num_entries = buf_rdr.read_u32::<Be>()?;

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

        Ok(Self(map))
    }

    /// Returns the number of bytes that will be written by [`write_body`](Information::write_body).
    pub fn write_body_size(&self) -> i64 {
        let entries_size = self
            .0
            .iter()
            .map(|(k, v)| k.len() + v.len() + 2)
            .sum::<usize>() as i64;

        4 + entries_size
    }

    /// Writes this chunk's body to a writer.
    pub fn write_body<T: Write>(&self, mut wtr: T) -> Result<(), CafWriteError> {
        wtr.write_u32::<Be>(self.0.len() as u32)?;
        for (k, v) in self.0.iter() {
            wtr.write_all(k.as_bytes())?;
            wtr.write_all(&[0x00])?;
            wtr.write_all(v.as_bytes())?;
            wtr.write_all(&[0x00])?;
        }
        Ok(())
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

    Err(read_invalid!("unterminated variable-length integer"))
}

/// Returns the size in bytes of a variable-length quantity if written.
fn vlq_size(mut val: u64) -> usize {
    let mut len = 0;
    loop {
        val >>= 7;
        len += 1;
        if val == 0 {
            break;
        }
    }
    len
}

/// Writes a variable-length quantity to a [`Write`].
fn write_vlq<T: Write>(mut wtr: T, mut val: u64) -> Result<(), CafWriteError> {
    const BUF_SIZE: usize = 10;
    let mut buf = [0; BUF_SIZE];
    let mut len = 0;
    loop {
        // take low 7 bits from val
        let mut byte = (val & 0b0111_1111) as u8;
        // set continuation bit high
        byte |= 0b1000_0000;
        // write bytes into buf starting from the end
        buf[BUF_SIZE - len - 1] = byte;
        len += 1;
        // shift val right 7
        val >>= 7;
        // break if val == 0
        if val == 0 {
            break;
        }
    }
    // set continuation bit of last byte low
    buf[9] &= 0b0111_1111;
    // write `len` bytes from the end of `buf`
    wtr.write_all(&buf[(BUF_SIZE - len)..])?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, io::Cursor};

    fn test_read_vlq(bytes: &[u8], expected: u64) {
        let mut cursor = Cursor::new(Vec::from(bytes));
        let res = super::read_vlq(&mut cursor);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), expected);
    }

    #[test]
    fn read_vlq() {
        // examples from the spec
        test_read_vlq(&[0x01], 1);
        test_read_vlq(&[0x11], 17);
        test_read_vlq(&[0x7f], 127);
        test_read_vlq(&[0x81, 0x00], 128);
        test_read_vlq(&[0x81, 0x02], 130);
        test_read_vlq(&[0x82, 0x01], 257);
        test_read_vlq(&[0xff, 0x7f], 16383);
        test_read_vlq(&[0x81, 0x80, 0x00], 16384);
    }

    fn test_vlq_size(val: u64, expected: usize) {
        let res = super::vlq_size(val);
        assert_eq!(res, expected);
    }

    #[test]
    fn vlq_size() {
        // examples from the spec
        test_vlq_size(1, 1);
        test_vlq_size(17, 1);
        test_vlq_size(127, 1);
        test_vlq_size(128, 2);
        test_vlq_size(130, 2);
        test_vlq_size(257, 2);
        test_vlq_size(16383, 2);
        test_vlq_size(16384, 3);
    }

    fn test_write_vlq(val: u64, expected: &[u8]) {
        let mut buf = Vec::new();
        let res = super::write_vlq(&mut buf, val);
        assert!(res.is_ok());
        assert_eq!(buf, expected);
    }

    #[test]
    fn write_vlq() {
        // examples from the spec
        test_write_vlq(1, &[0x01]);
        test_write_vlq(17, &[0x11]);
        test_write_vlq(127, &[0x7f]);
        test_write_vlq(128, &[0x81, 0x00]);
        test_write_vlq(130, &[0x81, 0x02]);
        test_write_vlq(257, &[0x82, 0x01]);
        test_write_vlq(16383, &[0xff, 0x7f]);
        test_write_vlq(16384, &[0x81, 0x80, 0x00]);
    }

    #[test]
    fn unterminated_vlq() {
        let mut cursor = Cursor::new(&[0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]);
        assert!(super::read_vlq(&mut cursor).is_err());
    }

    #[test]
    fn write_information_chunk() {
        let mut map = HashMap::new();
        map.insert("encoder".to_string(), "Lavf58.29.100".to_string());

        let information = super::Information(map);

        let mut buf = Vec::new();
        let header = super::ChunkHeader {
            chunk_type: crate::ChunkType::Information,
            chunk_size: information.write_body_size(),
        };
        header.write(&mut buf).unwrap();
        information.write_body(&mut buf).unwrap();

        assert_eq!(
            buf,
            vec![
                0x69, 0x6E, 0x66, 0x6F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1A, 0x00, 0x00,
                0x00, 0x01, 0x65, 0x6E, 0x63, 0x6F, 0x64, 0x65, 0x72, 0x00, 0x4C, 0x61, 0x76, 0x66,
                0x35, 0x38, 0x2E, 0x32, 0x39, 0x2E, 0x31, 0x30, 0x30, 0x00
            ]
        )
    }
}
