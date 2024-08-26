// CAF container decoder written in Rust
//
// This example file is licensed under the CC-0 license:
// https://creativecommons.org/publicdomain/zero/1.0/

extern crate caf;
use caf::chunks::{AudioDescription, Chunk, Information};
use caf::reading::PacketReader;
use caf::writing::PacketWriter;
use caf::{ChunkType, FormatType};
use std::collections::HashMap;
use std::io::Cursor;

fn main() {
    // Construct a PacketWriter.
    let mut buf = Vec::new();
    let audio_description = AudioDescription {
        sample_rate: 48000.0,
        format_id: FormatType::Other(u32::from_be_bytes(*b"opus")),
        format_flags: 0,
        bytes_per_packet: 0,
        frames_per_packet: 960,
        channels_per_frame: 2,
        bits_per_channel: 0,
    };
    let mut wtr = PacketWriter::new(&mut buf, &audio_description).unwrap();

    // Write an Information chunk to the file.
    let mut map = HashMap::new();
    map.insert("foo".to_string(), "bar".to_string());
    let information_chunk = Chunk::Information(Information(map));
    wtr.write_chunk(&information_chunk).unwrap();

    // Add some packets and write the Audio Data chunk.
    for _ in 0..5 {
        wtr.add_packet(&[0x12, 0x34, 0x56], None).unwrap();
    }
    wtr.write_audio_data().unwrap();

    // Now read the buffer and print some details.
    let rdr = PacketReader::new(Cursor::new(buf), &[ChunkType::Information]).unwrap();
    println!("Chunks: {:?}", rdr.extra_chunks);
    println!("Num packets: {:?}", rdr.num_packets());
}
