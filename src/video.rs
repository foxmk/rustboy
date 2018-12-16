use crate::memory;
use std::ops::RangeInclusive;

pub const VIDEO_RAM_START: u16 = 0x8000;
pub const VIDEO_RAM_END: u16 = 0x9FFF;
pub const OAM_START: u16 = 0xFE00;
pub const OAM_END: u16 = 0xFE9F;

pub struct Gpu {}

impl Gpu {
    pub fn new() -> Self {
        Self {}
    }
}

impl memory::Addressable for Gpu {
    fn address_ranges(&self) -> Vec<RangeInclusive<u16>> {
        vec![
            VIDEO_RAM_START..=VIDEO_RAM_END,
            OAM_START..=OAM_END,
        ]
    }

    fn get(&self, address: u16) -> Result<u8, memory::Error> {
        unimplemented!()
    }

    fn set(&mut self, address: u16, byte: u8) -> Result<(), memory::Error> {
        unimplemented!()
    }
}
