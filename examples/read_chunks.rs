// CAF container decoder written in Rust
//
// This example file is licensed under the CC-0 license:
// https://creativecommons.org/publicdomain/zero/1.0/

extern crate caf;
use caf::reading::ChunkReader;
use std::env;
use std::fs::File;

fn main() {
    let file_path = env::args()
        .nth(1)
        .expect("No arg found. Please specify a file to open.");

    println!("Opening file: {}", file_path);
    let file = File::open(file_path).unwrap();

    // Construct a ChunkReader from the file.
    let mut rdr = ChunkReader::new(file).unwrap();

    // Decode the chunks and print them.
    while let Some(chunk) = rdr.read_chunk().unwrap() {
        println!("{:?}", chunk);
    }
}
