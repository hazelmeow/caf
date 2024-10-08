// CAF container decoder written in Rust
//
// Copyright (c) 2017 est31 <MTest31@outlook.com>
// and contributors. All rights reserved.
// Licensed under MIT license, or Apache 2 license,
// at your option. Please see the LICENSE file
// attached to this source distribution for details.

#![allow(clippy::mistyped_literal_suffixes)]

/// Module containing constants for the chunk types defined by the spec.
///
/// Note that the list in the spec is explicitly non-exhaustive.
mod chunk_types {
    // The order here is the order they appear in the spec.
    // The spec is non-exhaustive, so we have both the constants and the enum.
    //
    // Applications are also allowed to define their own custom chunk types
    // if they are outside the space of reserved identifiers.

    pub const AUDIO_DESCRIPTION: u32 = 0x64_65_73_63; // "desc"
    pub const AUDIO_DATA: u32 = 0x64_61_74_61; // "data"
    pub const PACKET_TABLE: u32 = 0x70_61_6b_74; // "pakt"
    pub const CHANNEL_LAYOUT: u32 = 0x63_68_61_6e; // "chan"
    pub const MAGIC_COOKIE: u32 = 0x6b_75_6b_69; // "kuki"
    pub const STRINGS: u32 = 0x73_74_42_67; // "strg"
    pub const MARKER: u32 = 0x6d_61_72_6b; // "mark"
    pub const REGION: u32 = 0x72_65_67_6e; // "regn"
    pub const INSTRUMENT: u32 = 0x69_6e_73_74; // "inst"
    pub const MIDI: u32 = 0x6d_69_64_69; // "midi"
    pub const OVERVIEW: u32 = 0x6f_76_76_77; // "ovvw"
    pub const PEAK: u32 = 0x70_65_61_6b; // "peak"
    pub const EDIT_COMMENTS: u32 = 0x65_64_63_74; // "edct"
    pub const INFORMATION: u32 = 0x69_6e_66_6f; // "info"
    pub const UNIQUE_MATERIAL_IDENTIFIER: u32 = 0x75_6d_69_64; // "umid"
    pub const USER_DEFINED: u32 = 0x75_75_69_64; // "uuid"
    pub const FREE: u32 = 0x66_72_65_65; // "free"
}

/// `ChunkType` enumerates standard chunk types.
///
/// The list of chunk types in the spec is explicitly non-exhaustive,
/// and applications are allowed to define their own chunk types
/// outside the space of reserved identifiers.
/// Those chunk types are represented by the [`Other`](ChunkType::Other) variant.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChunkType {
    /// mChunkType for the "Audio Description" chunk
    AudioDescription,
    /// mChunkType for the "Audio Data" chunk
    AudioData,
    /// mChunkType for the "Packet Table" chunk
    PacketTable,
    /// mChunkType for the "Channel Layout" chunk
    ChannelLayout,
    /// mChunkType for the "Magic Cookie" chunk
    MagicCookie,
    /// mChunkType for the "Strings" chunk
    Strings,
    /// mChunkType for the "Marker" chunk
    Marker,
    /// mChunkType for the "Region" chunk
    Region,
    /// mChunkType for the "Instrument" chunk
    Instrument,
    /// mChunkType for the "MIDI" chunk
    Midi,
    /// mChunkType for the "Overview" chunk
    Overview,
    /// mChunkType for the "Peak" chunk
    Peak,
    /// mChunkType for the "Edit Comments" chunk
    EditComments,
    /// mChunkType for the "Information" chunk
    Information,
    /// mChunkType for the "Unique Material Identifier" chunk
    UniqueMaterialIdentifier,
    /// mChunkType for the "User-Defined" chunk
    UserDefined,
    /// mChunkType for the "Free" chunk
    Free,
    /// Variant for all other chunk types.
    Other(u32),
}

impl ChunkType {
    /// Returns the u32 representation of this [`ChunkType`].
    pub fn as_u32(&self) -> u32 {
        use self::chunk_types::*;
        use self::ChunkType::*;
        match self {
            AudioDescription => AUDIO_DESCRIPTION,
            AudioData => AUDIO_DATA,
            PacketTable => PACKET_TABLE,
            ChannelLayout => CHANNEL_LAYOUT,
            MagicCookie => MAGIC_COOKIE,
            Strings => STRINGS,
            Marker => MARKER,
            Region => REGION,
            Instrument => INSTRUMENT,
            Midi => MIDI,
            Overview => OVERVIEW,
            Peak => PEAK,
            EditComments => EDIT_COMMENTS,
            Information => INFORMATION,
            UniqueMaterialIdentifier => UNIQUE_MATERIAL_IDENTIFIER,
            UserDefined => USER_DEFINED,
            Free => FREE,
            Other(v) => *v,
        }
    }
}

impl From<u32> for ChunkType {
    fn from(v: u32) -> Self {
        use self::chunk_types::*;
        use self::ChunkType::*;
        match v {
            AUDIO_DESCRIPTION => AudioDescription,
            AUDIO_DATA => AudioData,
            PACKET_TABLE => PacketTable,
            CHANNEL_LAYOUT => ChannelLayout,
            MAGIC_COOKIE => MagicCookie,
            STRINGS => Strings,
            MARKER => Marker,
            REGION => Region,
            INSTRUMENT => Instrument,
            MIDI => Midi,
            OVERVIEW => Overview,
            PEAK => Peak,
            EDIT_COMMENTS => EditComments,
            INFORMATION => Information,
            UNIQUE_MATERIAL_IDENTIFIER => UniqueMaterialIdentifier,
            USER_DEFINED => UserDefined,
            FREE => Free,
            _ => Other(v),
        }
    }
}

/// Module containing constants for Audio Description chunk format IDs defined by the spec.
///
/// Note that the list in the spec is explicitly non-exhaustive.
mod format_types {
    // The order here is the order they appear in the spec.
    // The spec is non-exhaustive, so we have both the constants and the enum.

    pub const LINEAR_PCM: u32 = 0x6c_70_63_6d; // "lpcm"
    pub const APPLE_IMA4: u32 = 0x69_6d_61_34; // "ima4"
    pub const MPEG4_AAC: u32 = 0x61_61_63_20; // "aac "
    pub const MACE3: u32 = 0x4d_41_43_33; // "MAC3"
    pub const MACE6: u32 = 0x4d_41_43_36; // "MAC6"
    pub const U_LAW: u32 = 0x75_6c_61_77; // "ulaw"
    pub const A_LAW: u32 = 0x61_6c_61_77; // "alaw"
    pub const MPEG_LAYER_1: u32 = 0x2e_6d_70_31; // ".mp1"
    pub const MPEG_LAYER_2: u32 = 0x2e_6d_70_32; // ".mp2"
    pub const MPEG_LAYER_3: u32 = 0x2e_6d_70_33; // ".mp3"
    pub const AAPL_LOSSLESS: u32 = 0x61_6c_61_63; // "alac"
}

/// `FormatType` enumerates standard format ID values.
///
/// This enum lists `mFormatID` values defined by the spec.
/// The list of values in the spec is explicitly non-exhaustive.
/// Other values are represented by the [`Other`](FormatType::Other) variant.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormatType {
    /// mFormatID for Linear PCM
    LinearPcm,
    /// mFormatID for IMA 4:1 ADPCM
    AppleIma4,
    /// mFormatID for MPEG-4 AAC
    Mpeg4Aac,
    /// mFormatID for MACE 3:1
    Mace3,
    /// mFormatID for MACE 6:1
    Mace6,
    /// mFormatID for uLaw 2:1
    Ulaw,
    /// mFormatID for aLaw 2:1
    Alaw,
    /// mFormatID for MPEG-1
    MpegLayer1,
    /// mFormatID for MPEG-{1,2}
    MpegLayer2,
    /// mFormatID for MPEG-{1,2,3}
    MpegLayer3,
    /// mFormatID for Apple Lossless
    AppleLossless,
    /// Variant for all other values.
    Other(u32),
}

impl FormatType {
    /// Returns the u32 representation of this [`FormatType`].
    pub fn as_u32(&self) -> u32 {
        use self::format_types::*;
        use self::FormatType::*;
        match self {
            LinearPcm => LINEAR_PCM,
            AppleIma4 => APPLE_IMA4,
            Mpeg4Aac => MPEG4_AAC,
            Mace3 => MACE3,
            Mace6 => MACE6,
            Ulaw => U_LAW,
            Alaw => A_LAW,
            MpegLayer1 => MPEG_LAYER_1,
            MpegLayer2 => MPEG_LAYER_2,
            MpegLayer3 => MPEG_LAYER_3,
            AppleLossless => AAPL_LOSSLESS,
            Other(v) => *v,
        }
    }
}

impl From<u32> for FormatType {
    fn from(v: u32) -> Self {
        use self::format_types::*;
        use self::FormatType::*;
        match v {
            LINEAR_PCM => LinearPcm,
            APPLE_IMA4 => AppleIma4,
            MPEG4_AAC => Mpeg4Aac,
            MACE3 => Mace3,
            MACE6 => Mace6,
            U_LAW => Ulaw,
            A_LAW => Alaw,
            MPEG_LAYER_1 => MpegLayer1,
            MPEG_LAYER_2 => MpegLayer2,
            MPEG_LAYER_3 => MpegLayer3,
            AAPL_LOSSLESS => AppleLossless,
            _ => Other(v),
        }
    }
}
