// CAF container decoder written in Rust
//
// Copyright (c) 2017 est31 <MTest31@outlook.com>
// and contributors. All rights reserved.
// Licensed under MIT license, or Apache 2 license,
// at your option. Please see the LICENSE file
// attached to this source distribution for details.

//! A Core Audio Format (CAF) container decoder and encoder.
//!
//! For more information on CAF, see its [Wikipedia page](https://en.wikipedia.org/wiki/Core_Audio_Format)
//! and the [official specification](https://developer.apple.com/library/archive/documentation/MusicAudio/Reference/CAFSpec/CAF_intro/CAF_intro.html).

#![forbid(unsafe_code)]

mod enums;
pub use enums::{ChunkType, FormatType};

pub mod chunks;
pub mod reading;
