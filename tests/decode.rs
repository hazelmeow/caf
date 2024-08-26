use caf::{
    chunks::{AudioData, Chunk},
    reading::{ChunkReader, PacketReader},
    ChunkType, FormatType,
};
use std::io::{Cursor, Read};

const OPUS_FILE: &[u8] = include_bytes!("./opus.caf");
const PCM_FILE: &[u8] = include_bytes!("./pcm.caf");
const ALAC_FILE: &[u8] = include_bytes!("./alac.caf");

const OPUS_PACKET_16: &[u8] = &[
    0xFC, 0x7F, 0xFB, 0xD9, 0x85, 0x0F, 0x0C, 0x2A, 0x6A, 0xC8, 0x41, 0x4E, 0xCC, 0xF9, 0x43, 0x65,
    0x29, 0xAE, 0x72, 0xA4, 0xF5, 0x16, 0xE9, 0x7F, 0x7C, 0xE0, 0x1E, 0xC8, 0xC5, 0xBB, 0xAB, 0x46,
    0xBC, 0x7C, 0xB0, 0x6A, 0x6E, 0x67, 0x03, 0xAC, 0xB2, 0x9F, 0x67, 0xC6, 0x70, 0x61, 0x2C, 0x2C,
    0x7E, 0xCB, 0xA1, 0x31, 0x83, 0x9D, 0xAF, 0xE7, 0xD0, 0x66, 0x2E, 0xA0, 0x70, 0x41, 0xEB, 0x3A,
    0x42, 0x57, 0xA8, 0xDD, 0xC7, 0x33, 0x7C, 0x3E, 0xED, 0x10, 0xA4, 0x15, 0x6A, 0x0B, 0xD1, 0x31,
    0x7D, 0x83, 0xEE, 0x92, 0x69, 0x32, 0x37, 0x2C, 0x94, 0x89, 0xA9, 0x99, 0xE6, 0x65, 0x60, 0xEB,
    0xDD, 0xDD, 0x77, 0x2F, 0xF8, 0xC5, 0x55, 0x39, 0x85, 0x7A, 0xCE, 0xCC, 0x35, 0xA8, 0xA5, 0xCC,
    0x8F, 0x86, 0x72, 0xA8, 0xCC, 0x34, 0x5E, 0x1E, 0xEA, 0x04, 0x10, 0x4F, 0x98, 0x93, 0x8C, 0x72,
    0x28, 0x79, 0xAF, 0x8B, 0x88, 0x08, 0xCC, 0x16, 0xED, 0x9B, 0x23, 0xB0, 0xC7, 0x4D, 0xC7, 0xE6,
    0x15, 0x0B, 0x63, 0x7F, 0x6E, 0x4F, 0x1B, 0x1E, 0xA0, 0x52, 0xA0, 0x24, 0xE6, 0x43, 0xE1, 0xB5,
    0xE4, 0x56, 0x90, 0x63, 0x54, 0x5E, 0x96, 0xB4, 0x4A, 0x78, 0xD6, 0x99, 0x1A, 0x35, 0xC3, 0xD9,
    0xA5, 0x97, 0xAE, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0F, 0x44, 0x1E, 0xA4, 0x15, 0x10, 0x4F, 0x52,
    0xE3, 0x6B, 0xA5, 0xD4, 0x46, 0x75, 0x1E, 0x46, 0x75, 0x1E, 0x51, 0x58, 0xE7, 0xE1, 0x87, 0xD8,
    0x57, 0x95, 0x41, 0xCF, 0x95, 0x41, 0xC9, 0x8F, 0x6A, 0xBB, 0x0A, 0x1C, 0xC6, 0x44, 0xB0, 0xC1,
    0x44, 0xB0, 0xC1, 0x6F, 0x2E, 0x0A, 0xF0, 0x11, 0x28, 0xF9, 0x44, 0xFE, 0x39, 0x44, 0xFE, 0x37,
    0x22, 0xF7, 0x22, 0xF7, 0x22, 0xF7, 0x22, 0xEB, 0x69, 0x8B, 0x69, 0x8B, 0x69, 0x8D, 0xBF, 0x86,
    0xE5, 0x39, 0xA2, 0x7F, 0x62, 0x61, 0x21, 0xAB, 0x7E, 0x16, 0x97, 0x94, 0xD8, 0x4D, 0x01, 0xB4,
    0x06, 0x10, 0x20, 0x44, 0x7E, 0x29, 0x3F, 0xA3, 0x84, 0x0A, 0x3D, 0x67, 0x89, 0x6F, 0x3E, 0x4A,
    0xC7, 0x2B, 0xAC, 0xD8, 0x41, 0xF8, 0x59, 0x39, 0xAE, 0xDE, 0xF2, 0xC9, 0x94, 0x6B, 0x53, 0x07,
    0x57, 0x95, 0x0C, 0x7C, 0x69, 0x9E, 0x08, 0x80, 0x33, 0xD2, 0x9A, 0xE2, 0x16, 0x95, 0xF6, 0xEA,
    0xB9, 0x6E, 0x5B, 0x96, 0xE5, 0x8B, 0xFE, 0x1C, 0x73, 0xDE, 0xE5, 0xB9, 0x6E, 0x5B, 0x97, 0x0B,
    0x10, 0xD0, 0x16, 0xC6, 0x44, 0xC8, 0x99, 0x12, 0x66, 0x49, 0xFD, 0x29, 0xD3, 0x3D, 0x01, 0x35,
    0x6A, 0xFC, 0x3C, 0xB3, 0xAC, 0xB5, 0xA0, 0xDB, 0xD0, 0xA5, 0xC7, 0x75, 0x38, 0xE9, 0x70, 0x4A,
    0xC4, 0x8C, 0xB4, 0x45, 0xDD, 0x01, 0x57, 0x80, 0x33, 0xD3, 0x80, 0xB2, 0x72, 0xD7, 0xE8, 0x41,
    0xE5, 0xF2, 0x1A, 0xD1, 0xDF, 0x47, 0xE0, 0xB3, 0xE4, 0x04, 0xA3, 0xE5, 0xDF, 0x04, 0x5A, 0xD1,
    0xFB, 0x28, 0xA3, 0xE5, 0xDD, 0xBA, 0x84, 0x06, 0x06, 0x1C, 0x60, 0x87, 0x08, 0x50, 0x86, 0x18,
    0x11, 0x85, 0x33, 0xD7, 0x6D, 0xEC,
];
const OPUS_PACKET_17: &[u8] = &[
    0xFC, 0x7F, 0xFD, 0xD0, 0x95, 0xA0, 0xDA, 0x11, 0xB7, 0xEE, 0xBC, 0x57, 0x2A, 0x97, 0x4C, 0x37,
    0x7A, 0x8A, 0xF3, 0x5C, 0xCE, 0x60, 0xC4, 0x06, 0xEE, 0x7D, 0x67, 0x43, 0x9D, 0x61, 0x57, 0xDE,
    0xBD, 0x45, 0x81, 0x22, 0xD8, 0xCC, 0x95, 0xB3, 0x26, 0x54, 0xCE, 0x58, 0xBC, 0xAB, 0xA1, 0x48,
    0xB0, 0xB1, 0x60, 0xE6, 0x5A, 0xC5, 0x0C, 0xC8, 0x1A, 0xA8, 0xB1, 0xC9, 0x77, 0x4B, 0xC9, 0x24,
    0x70, 0xDD, 0xB6, 0x54, 0x4C, 0xC9, 0xBF, 0x93, 0xF1, 0x1E, 0x28, 0x1B, 0x76, 0x63, 0x69, 0x6C,
    0x4D, 0x34, 0xF1, 0xAA, 0x07, 0xBC, 0x60, 0xE2, 0x76, 0xB7, 0xE6, 0x49, 0x51, 0x45, 0xBA, 0x7D,
    0x72, 0x4B, 0x1E, 0x8F, 0x55, 0xE2, 0x7D, 0x84, 0x25, 0xC8, 0x06, 0x93, 0xBF, 0x3D, 0xC8, 0x00,
    0x04, 0x00, 0x07, 0xAB, 0xFD, 0x30, 0x05, 0xD1, 0xE0, 0x00, 0x00, 0x84, 0x01, 0x31, 0x24, 0x1F,
    0xC1,
];

const PCM_PACKET_32768: &[u8] = &[0xBF, 0x04, 0xDC, 0xFB];
const PCM_PACKET_32769: &[u8] = &[0xC7, 0x04, 0xF4, 0xFB];

const ALAC_MAGIC_COOKIE: &[u8] = &[
    0x00, 0x00, 0x00, 0x0C, 0x66, 0x72, 0x6D, 0x61, 0x61, 0x6C, 0x61, 0x63, 0x00, 0x00, 0x00, 0x24,
    0x61, 0x6C, 0x61, 0x63, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x18, 0x28, 0x0A,
    0x0E, 0x02, 0x00, 0x00, 0x00, 0x00, 0x60, 0x04, 0x00, 0x23, 0x28, 0x00, 0x00, 0x00, 0xBB, 0x80,
];

#[test]
fn read_audio_description_chunk() {
    let rdr = Cursor::new(OPUS_FILE);
    let mut ch_rdr = ChunkReader::new(rdr).expect("should construct");

    let chunk = ch_rdr
        .read_chunk()
        .expect("should succeed")
        .expect("should be present");

    assert_eq!(chunk.chunk_type(), ChunkType::AudioDescription);

    let Chunk::AudioDescription(chunk) = chunk else {
        unreachable!()
    };

    assert_eq!(chunk.sample_rate, 48000.0);
    assert_eq!(chunk.format_id, FormatType::Other(0x6f_70_75_73));
    assert_eq!(chunk.format_flags, 0);
    assert_eq!(chunk.bytes_per_packet, 0);
    assert_eq!(chunk.frames_per_packet, 960);
    assert_eq!(chunk.channels_per_frame, 2);
    assert_eq!(chunk.bits_per_channel, 0);
}

#[test]
fn read_all_chunks() {
    let rdr = Cursor::new(OPUS_FILE);
    let mut ch_rdr = ChunkReader::new(rdr).expect("should construct");

    let mut chunk_types = Vec::new();
    while let Some(chunk) = ch_rdr.read_chunk().expect("should succeed") {
        chunk_types.push(chunk.chunk_type());
    }

    assert_eq!(
        chunk_types,
        vec![
            ChunkType::AudioDescription,
            ChunkType::ChannelLayout,
            ChunkType::Information,
            ChunkType::AudioData,
            ChunkType::PacketTable,
        ]
    )
}

#[test]
fn read_chunk_raw() {
    let rdr = Cursor::new(OPUS_FILE);
    let mut ch_rdr: ChunkReader<Cursor<&[u8]>> = ChunkReader::new(rdr).expect("should construct");

    // read header
    let header = ch_rdr
        .read_chunk_header()
        .expect("should succeed")
        .expect("should be present");

    // read raw
    let body = ch_rdr.read_chunk_raw(&header).expect("should succeed");
    assert_eq!(
        body,
        vec![
            0x40, 0xE7, 0x70, 0x00, 0x00, 0x00, 0x00, 0x00, 0x6F, 0x70, 0x75, 0x73, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0xC0, 0x00, 0x00, 0x00, 0x02,
            0x00, 0x00, 0x00, 0x00,
        ]
    );

    // next chunk should read correctly
    ch_rdr
        .read_chunk()
        .expect("should succeed")
        .expect("should be present");
}

#[test]
fn seek_to_next_chunk() {
    let rdr = Cursor::new(OPUS_FILE);
    let mut ch_rdr = ChunkReader::new(rdr).expect("should construct");

    // 1st chunk
    let c1 = ch_rdr
        .read_chunk()
        .expect("should succeed")
        .expect("should be present");
    assert_eq!(c1.chunk_type(), ChunkType::AudioDescription);

    // skip 2nd chunk
    let c2_header = ch_rdr
        .read_chunk_header()
        .expect("should succeed")
        .expect("should be present");
    ch_rdr
        .seek_to_next_chunk(&c2_header)
        .expect("should succeed");

    // 3rd chunk
    let c3 = ch_rdr
        .read_chunk()
        .expect("should succeed")
        .expect("should be present");
    assert_eq!(c3.chunk_type(), ChunkType::Information);
}

#[test]
fn read_audio_data() {
    let rdr = Cursor::new(OPUS_FILE);
    let mut ch_rdr = ChunkReader::new(rdr).expect("should construct");

    // loop until we reach the audio data chunk, read it, and keep the stream positioned at the start of the audio data
    let mut audio_data = None;
    while let Some(header) = ch_rdr.read_chunk_header().expect("should succeed") {
        if header.chunk_type == ChunkType::AudioData {
            // read until data starts
            let chunk = AudioData::read_until_data(ch_rdr.get_mut(), header.chunk_size)
                .expect("should succeed");
            audio_data = Some(chunk);
            break;
        } else {
            // skip chunk data
            ch_rdr.seek_to_next_chunk(&header).expect("should succeed");
        }
    }

    // should have found the Audio Data chunk
    assert!(audio_data.is_some());

    // read some audio data
    let mut inner_rdr = ch_rdr.into_inner();
    let mut buf = vec![0; 32];
    inner_rdr.read_exact(&mut buf).expect("should succeed");

    assert_eq!(
        buf,
        vec![
            0xFC, 0xFF, 0xFE, 0xFC, 0xFF, 0xFE, 0xFC, 0xFF, 0xFE, 0xFC, 0xFF, 0xFE, 0xFC, 0xFF,
            0xFE, 0xFC, 0xFF, 0xFE, 0xFC, 0xFF, 0xFE, 0xFC, 0xFF, 0xFE, 0xFC, 0xFF, 0xFE, 0xFC,
            0xFF, 0xFE, 0xFC, 0xFF,
        ],
    )
}

#[test]
fn not_caf() {
    let rdr = Cursor::new(vec![0; 100]);
    let ch_rdr_res = ChunkReader::new(rdr);
    assert!(ch_rdr_res.is_err());
}

#[test]
fn read_packets() {
    let rdr = Cursor::new(OPUS_FILE);
    let mut packet_rdr = PacketReader::new(rdr, &[]).expect("should succeed");

    // opus
    assert!(packet_rdr.has_variable_packet_bytes());
    assert!(!packet_rdr.has_variable_packet_frames());

    // idx/size of first packet
    assert_eq!(packet_rdr.next_packet_idx(), 0);
    assert_eq!(packet_rdr.next_packet_size(), Some(3));

    // read first packet
    let packet = packet_rdr
        .read_packet()
        .expect("should succeed")
        .expect("should be present");
    assert_eq!(packet, vec![0xFC, 0xFF, 0xFE]);

    // idx/size of first packet
    assert_eq!(packet_rdr.next_packet_idx(), 1);
    assert_eq!(packet_rdr.next_packet_size(), Some(3));

    // read second packet
    let packet = packet_rdr
        .read_packet()
        .expect("should succeed")
        .expect("should be present");
    assert_eq!(packet, vec![0xFC, 0xFF, 0xFE]);

    // read more packets
    let packets = vec![
        vec![0xFC, 0xFF, 0xFE],
        vec![0xFC, 0xFF, 0xFE],
        vec![0xFC, 0xFF, 0xFE],
        vec![0xFC, 0xFF, 0xFE],
        vec![0xFC, 0xFF, 0xFE],
        vec![0xFC, 0xFF, 0xFE],
        vec![0xFC, 0xFF, 0xFE],
        vec![0xFC, 0xFF, 0xFE],
        vec![0xFC, 0xFF, 0xFE],
        vec![0xFC, 0xFF, 0xFE],
        vec![0xFC, 0xFF, 0xFE],
        vec![0xFC, 0xFF, 0xFE],
        vec![0xFC, 0xFF, 0xFE],
        vec![0xFC, 0xFF, 0xFE],
        OPUS_PACKET_16.into(),
        OPUS_PACKET_17.into(),
    ];
    for expected in packets {
        let packet = packet_rdr
            .read_packet()
            .expect("should succeed")
            .expect("should be present");
        assert_eq!(packet, expected);
    }
}

#[test]
fn read_all_packets_opus() {
    let rdr = Cursor::new(OPUS_FILE);
    let mut packet_rdr = PacketReader::new(rdr, &[]).expect("should succeed");

    // should read all
    for _ in 0..251 {
        assert!(packet_rdr.next_packet_size().is_some());
        packet_rdr
            .read_packet()
            .expect("should succeed")
            .expect("should be present");
    }

    // should be done and return Ok(None)
    assert!(packet_rdr.next_packet_size().is_none());
    assert!(packet_rdr.read_packet().expect("should succeed").is_none());
}

#[test]
fn read_all_packets_pcm() {
    let rdr = Cursor::new(PCM_FILE);
    let mut packet_rdr = PacketReader::new(rdr, &[]).expect("should succeed");

    // should read all
    for _ in 0..240000 {
        assert!(packet_rdr.next_packet_size().is_some());
        packet_rdr
            .read_packet()
            .expect("should succeed")
            .expect("should be present");
    }

    // should be done and return Ok(None)
    // since pcm is CBR, next_packet_size will always return Some so we can't use it to check for EOF
    assert!(packet_rdr.read_packet().expect("should succeed").is_none());
}

#[test]
fn read_all_packets_alac() {
    let rdr = Cursor::new(ALAC_FILE);
    let mut packet_rdr = PacketReader::new(rdr, &[]).expect("should succeed");

    // should read all
    for _ in 0..59 {
        assert!(packet_rdr.next_packet_size().is_some());
        packet_rdr
            .read_packet()
            .expect("should succeed")
            .expect("should be present");
    }

    // should be done and return Ok(None)
    assert!(packet_rdr.next_packet_size().is_none());
    assert!(packet_rdr.read_packet().expect("should succeed").is_none());
}

#[test]
fn seek_forward_to_packet_opus() {
    let rdr = Cursor::new(OPUS_FILE);
    let mut packet_rdr = PacketReader::new(rdr, &[]).expect("should succeed");

    // read some packets
    for _ in 0..5 {
        packet_rdr
            .read_packet()
            .expect("should succeed")
            .expect("should be present");
    }

    // seek to packet 16
    packet_rdr.seek_to_packet(16).expect("should succeed");

    // check index
    assert_eq!(packet_rdr.next_packet_idx(), 16);

    // read packets 16 and 17
    let packet_16 = packet_rdr
        .read_packet()
        .expect("should succeed")
        .expect("should be present");
    assert_eq!(packet_16, OPUS_PACKET_16);
    let packet_17 = packet_rdr
        .read_packet()
        .expect("should succeed")
        .expect("should be present");
    assert_eq!(packet_17, OPUS_PACKET_17);
}

#[test]
fn seek_forward_to_packet_pcm() {
    let rdr = Cursor::new(PCM_FILE);
    let mut packet_rdr = PacketReader::new(rdr, &[]).expect("should succeed");

    // read some packets
    for _ in 0..10000 {
        packet_rdr
            .read_packet()
            .expect("should succeed")
            .expect("should be present");
    }

    // seek to packet 32768
    packet_rdr.seek_to_packet(32768).expect("should succeed");

    // check index
    assert_eq!(packet_rdr.next_packet_idx(), 32768);

    // read packets 32768 and 32769
    let packet_32768 = packet_rdr
        .read_packet()
        .expect("should succeed")
        .expect("should be present");
    assert_eq!(packet_32768, PCM_PACKET_32768);
    let packet_32769 = packet_rdr
        .read_packet()
        .expect("should succeed")
        .expect("should be present");
    assert_eq!(packet_32769, PCM_PACKET_32769);
}

#[test]
fn seek_backward_to_packet_opus() {
    let rdr = Cursor::new(OPUS_FILE);
    let mut packet_rdr = PacketReader::new(rdr, &[]).expect("should succeed");

    // read many packets
    for _ in 0..50 {
        packet_rdr
            .read_packet()
            .expect("should succeed")
            .expect("should be present");
    }

    // seek to packet 16
    packet_rdr.seek_to_packet(16).expect("should succeed");

    // check index
    assert_eq!(packet_rdr.next_packet_idx(), 16);

    // read packets 16 and 17
    let packet_16 = packet_rdr
        .read_packet()
        .expect("should succeed")
        .expect("should be present");
    assert_eq!(packet_16, OPUS_PACKET_16);
    let packet_17 = packet_rdr
        .read_packet()
        .expect("should succeed")
        .expect("should be present");
    assert_eq!(packet_17, OPUS_PACKET_17);
}

#[test]
fn seek_backward_to_packet_pcm() {
    let rdr = Cursor::new(PCM_FILE);
    let mut packet_rdr = PacketReader::new(rdr, &[]).expect("should succeed");

    // read many packets
    for _ in 0..50000 {
        packet_rdr
            .read_packet()
            .expect("should succeed")
            .expect("should be present");
    }

    // seek to packet 32768
    packet_rdr.seek_to_packet(32768).expect("should succeed");

    // check index
    assert_eq!(packet_rdr.next_packet_idx(), 32768);

    // read packets 32768 and 32769
    let packet_32768 = packet_rdr
        .read_packet()
        .expect("should succeed")
        .expect("should be present");
    assert_eq!(packet_32768, PCM_PACKET_32768);
    let packet_32769 = packet_rdr
        .read_packet()
        .expect("should succeed")
        .expect("should be present");
    assert_eq!(packet_32769, PCM_PACKET_32769);
}

#[test]
fn read_alac_magic_cookie() {
    let rdr = Cursor::new(ALAC_FILE);

    // request Magic Cookie chunks
    let packet_rdr = PacketReader::new(rdr, &[ChunkType::MagicCookie]).expect("should succeed");

    assert_eq!(packet_rdr.extra_chunks.len(), 1);
    assert_eq!(
        packet_rdr.extra_chunks[0].chunk_type(),
        ChunkType::MagicCookie
    );

    let Chunk::MagicCookie(magic_cookie) = &packet_rdr.extra_chunks[0] else {
        unreachable!()
    };
    assert_eq!(magic_cookie, ALAC_MAGIC_COOKIE);
}

#[test]
fn get_num_packets() {
    let packet_rdr_opus = {
        let rdr = Cursor::new(OPUS_FILE);
        PacketReader::new(rdr, &[]).expect("should succeed")
    };
    assert_eq!(packet_rdr_opus.num_packets(), Some(251));

    let packet_rdr_pcm = {
        let rdr = Cursor::new(PCM_FILE);
        PacketReader::new(rdr, &[]).expect("should succeed")
    };
    assert_eq!(packet_rdr_pcm.num_packets(), Some(240000));

    let packet_rdr_alac = {
        let rdr = Cursor::new(ALAC_FILE);
        PacketReader::new(rdr, &[]).expect("should succeed")
    };
    assert_eq!(packet_rdr_alac.num_packets(), Some(59));
}
