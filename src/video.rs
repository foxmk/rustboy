use crate::memory;

use std::ops::RangeInclusive;

pub enum Error {}

enum GpuMode {}

enum GpuState {
    HBlank,
    VBlank,
    OAM,
    OAMRAM,
}

pub struct Gpu {
    vram: [u8; Gpu::VRAM_SIZE],
    oam: [u8; Gpu::OAM_SIZE],
}

impl Gpu {
    // Memory ranges
    const VIDEO_RAM_START: u16 = 0x8000;
    const VIDEO_RAM_END: u16 = 0x9FFF;
    const OAM_START: u16 = 0xFE00;
    const OAM_END: u16 = 0xFE9F;
    const GPU_IO_START: u16 = 0xFF40;
    const GPU_IO_END: u16 = 0xFE9F;

    // Memory size
    const OAM_SIZE: usize = (Gpu::OAM_END - Gpu::OAM_START + 1) as usize;
    const VRAM_SIZE: usize = (Gpu::VIDEO_RAM_END - Gpu::VIDEO_RAM_START + 1) as usize;

    // Memory registers
    const LCDC: u16 = 0xFF40;
    const STAT: u16 = 0xFF41;
    const SCY: u16 = 0xFF42;
    const SCX: u16 = 0xFF43;
    const LY: u16 = 0xFF44;
    const LYC: u16 = 0xFF45;
    const DMA: u16 = 0xFF46;
    const BGP: u16 = 0xFF47;
    const OBP0: u16 = 0xFF48;
    const OBP1: u16 = 0xFF49;
    const WY: u16 = 0xFF4A;
    const WX: u16 = 0xFF4B;

    pub fn new() -> Self {
        Self {
            vram: [0x00; Gpu::VRAM_SIZE],
            oam: [0x00; Gpu::OAM_SIZE],
        }
    }

    pub fn fill_bitmap(&self, buffer: &mut [u32]) -> Result<(), Error> {
        unimplemented!()
    }
}

impl memory::Addressable for Gpu {
    fn address_ranges(&self) -> Vec<RangeInclusive<u16>> {
        vec![
            Gpu::VIDEO_RAM_START..=Gpu::VIDEO_RAM_END,
            Gpu::OAM_START..=Gpu::OAM_END,
        ]
    }

    fn get(&self, address: u16) -> Result<u8, memory::Error> {
        unimplemented!()
    }

    fn set(&mut self, address: u16, byte: u8) -> Result<(), memory::Error> {
        unimplemented!()
    }
}
