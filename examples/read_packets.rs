// CAF container decoder written in Rust
//
// This example file is licensed under the CC-0 license:
// https://creativecommons.org/publicdomain/zero/1.0/

extern crate caf;
use caf::reading::PacketReader;
use std::env;
use std::fs::File;

fn main() {
    let file_path = env::args()
        .nth(1)
        .expect("No arg found. Please specify a file to open.");

    println!("Opening file: {}", file_path);
    let file = File::open(file_path).unwrap();

    // Construct a PacketReader from the file.
    let mut rdr = PacketReader::new(file, &[]).unwrap();

    // Print some decoded information about the file.
    println!("Sample rate: {:?}", rdr.audio_description.sample_rate);
    println!("Total packets: {:?}", rdr.num_packets());

    // Decode some packets and print them.
    for i in 0..5 {
        let packet = rdr.read_packet().unwrap();
        let packet_len = packet.map(|p| p.len());
        println!("Packet {i}: {packet_len:?} bytes");
    }
}
