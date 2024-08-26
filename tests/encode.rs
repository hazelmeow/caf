use caf::{
    chunks::AudioDescription, reading::PacketReader, writing::PacketWriter, ChunkType, FormatType,
};
use std::io::Cursor;

const OPUS_FILE: &[u8] = include_bytes!("./opus.caf");
const PCM_FILE: &[u8] = include_bytes!("./pcm.caf");
const ALAC_FILE: &[u8] = include_bytes!("./alac.caf");

const OPUS_AUDIO_DESCRIPTION: AudioDescription = AudioDescription {
    sample_rate: 48000.0,
    format_id: FormatType::Other(u32::from_be_bytes(*b"opus")),
    format_flags: 0,
    bytes_per_packet: 0,
    frames_per_packet: 960,
    channels_per_frame: 2,
    bits_per_channel: 0,
};

#[test]
fn encode_decode_packets() {
    let mut buf = Vec::new();

    let mut packet_writer = PacketWriter::new(&mut buf, &OPUS_AUDIO_DESCRIPTION).unwrap();

    // write some packets
    packet_writer
        .add_packet(&[0x10, 0x11, 0x12, 0x13], None)
        .unwrap();
    packet_writer
        .add_packet(&[0x20, 0x21, 0x22, 0x23], None)
        .unwrap();
    packet_writer
        .add_packet(&[0x30, 0x31, 0x32, 0x33], None)
        .unwrap();

    packet_writer.write_audio_data().unwrap();

    // decode and read packets
    let rdr = Cursor::new(&buf);
    let mut packet_reader = PacketReader::new(rdr, &[]).unwrap();

    assert_eq!(
        packet_reader.read_packet().unwrap(),
        Some(vec![0x10, 0x11, 0x12, 0x13])
    );
    assert_eq!(
        packet_reader.read_packet().unwrap(),
        Some(vec![0x20, 0x21, 0x22, 0x23])
    );
    assert_eq!(
        packet_reader.read_packet().unwrap(),
        Some(vec![0x30, 0x31, 0x32, 0x33])
    );
}

fn read_all_packets<T: std::io::Read + std::io::Seek>(
    packet_reader: &mut PacketReader<T>,
) -> Vec<Vec<u8>> {
    let mut packets = Vec::new();
    while let Some(packet) = packet_reader.read_packet().unwrap() {
        packets.push(packet);
    }
    packets
}

// won't produce identical output, but test that decoding/encoding/decoding again doesn't error
fn test_decode_encode_decode(file: &[u8]) {
    // read file
    let rdr = Cursor::new(file);
    let mut packet_reader = PacketReader::new(
        rdr,
        &[
            ChunkType::Information,
            ChunkType::ChannelLayout,
            ChunkType::MagicCookie,
        ],
    )
    .unwrap();
    let packets = read_all_packets(&mut packet_reader);

    let mut buf = Vec::new();
    // write
    {
        let mut packet_writer =
            PacketWriter::new(&mut buf, &packet_reader.audio_description).unwrap();

        // add extra decoded chunks
        for chunk in packet_reader.extra_chunks {
            match chunk.chunk_type() {
                ChunkType::ChannelLayout | ChunkType::Information | ChunkType::MagicCookie => {
                    packet_writer.write_chunk(&chunk).unwrap();
                }
                _ => {}
            }
        }

        for packet in packets {
            packet_writer.add_packet(&packet, None).unwrap();
        }
        packet_writer.write_audio_data().unwrap();
    }

    // shouldn't error
    let rdr2 = Cursor::new(buf);
    let mut packet_reader2 =
        PacketReader::new(rdr2, &[ChunkType::Information, ChunkType::ChannelLayout]).unwrap();
    read_all_packets(&mut packet_reader2);
}

#[test]
fn decode_encode_decode_opus() {
    test_decode_encode_decode(OPUS_FILE);
}
#[test]
fn decode_encode_decode_pcm() {
    test_decode_encode_decode(PCM_FILE);
}
#[test]
fn decode_encode_decode_alac() {
    test_decode_encode_decode(ALAC_FILE);
}

#[test]
fn edit_count() {
    let mut buf = Vec::new();
    let mut packet_writer = PacketWriter::new(&mut buf, &OPUS_AUDIO_DESCRIPTION).unwrap();

    // set edit count
    packet_writer.set_edit_count(1234);

    for _ in 0..5 {
        packet_writer.add_packet(&[0x12, 0x34, 0x56], None).unwrap();
    }
    packet_writer.write_audio_data().unwrap();

    let rdr = Cursor::new(buf);
    let packet_reader = PacketReader::new(rdr, &[]).unwrap();

    assert_eq!(packet_reader.audio_data.edit_count, 1234);
}

#[test]
fn priming_remainder_frames() {
    let mut buf = Vec::new();
    let mut packet_writer = PacketWriter::new(&mut buf, &OPUS_AUDIO_DESCRIPTION).unwrap();

    // set priming and remainder frames
    packet_writer.set_priming_frames(123);
    packet_writer.set_remainder_frames(456);

    for _ in 0..5 {
        packet_writer.add_packet(&[0x12, 0x34, 0x56], None).unwrap();
    }
    packet_writer.write_audio_data().unwrap();

    let rdr = Cursor::new(buf);
    let packet_reader = PacketReader::new(rdr, &[]).unwrap();

    let packet_table = packet_reader.packet_table.expect("should be present");
    assert_eq!(packet_table.priming_frames, 123);
    assert_eq!(packet_table.remainder_frames, 456);
}

// TODO: test with variable frames per packet (ex. vorbis)
