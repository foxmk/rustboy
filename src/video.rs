use crate::memory;

use std::ops::RangeInclusive;
use std::io::Write;

const VRAM_START: u16 = 0x8000;
const VRAM_END: u16 = 0x9FFF;
const OAM_START: u16 = 0xFE00;
const OAM_END: u16 = 0xFE9F;
const GPU_IO_START: u16 = 0xFF40;
const GPU_IO_END: u16 = 0xFF6B;

enum PpuMode {
    OAM { ticks_left: usize },
    OAMRAM { ticks_left: usize },
    HBlank { ticks_left: usize },
    VBlank { ticks_left: usize },
}

enum PpuMemReg {
    #[doc = "LCD Control Register (R/W)"] LCDC = 0xFF40,
    #[doc = "LCD Status (R/W)"] STAT = 0xFF41,
    #[doc = "Scroll Y (R/W)"] SCY = 0xFF42,
    #[doc = "Scroll X (R/W)"] SCX = 0xFF43,
    #[doc = "LCDC Y-Coordinate (R)"] LY = 0xFF44,
    #[doc = "LY Compare (R/W)"] LYC = 0xFF45,
    #[doc = "DMA Transfer and Start Address (R/W)"] DMA = 0xFF46,
    #[doc = "BG Palette Data (R/W)"] BGP = 0xFF47,
    #[doc = "Object Palette 0 Data (R/W)"] OBP0 = 0xFF48,
    #[doc = "Object Palette 1 Data (R/W)"] OBP1 = 0xFF49,
    #[doc = "Window Y Position (R/W)"] WY = 0xFF4A,
    #[doc = "Window X Position minus 7 (R/W)"] WX = 0xFF4B,
}


pub enum Error {}

struct LCDC {
// #[doc = "Bit 7 - LCD Display Enable             (0=Off, 1=On) "],
// #[doc = "Bit 6 - Window Tile Map Display Select (0=9800-9BFF, 1=9C00-9FFF) "],
// #[doc = "Bit 5 - Window Display Enable          (0=Off, 1=On) "],
// #[doc = "Bit 4 - BG & Window Tile Data Select   (0=8800-97FF, 1=8000-8FFF) "],
// #[doc = "Bit 3 - BG Tile Map Display Select     (0=9800-9BFF, 1=9C00-9FFF) "],
// #[doc = "Bit 2 - OBJ (Sprite) Size              (0=8x8, 1=8x16) "],
// #[doc = "Bit 1 - OBJ (Sprite) Display Enable    (0=Off, 1=On) "],
// #[doc = "Bit 0 - BG/Window Display/Priority     (0=Off, 1=On) "],
}

pub struct Ppu {
    ly: u8,
    lyc: u8,
    lx: u8,
    scx: u8,
    scy: u8,
    mode: PpuMode,
    vram: [u8; Ppu::VRAM_SIZE],
    oam: [u8; Ppu::OAM_SIZE],
    framebuffer: [[u32; Ppu::DISPLAY_WIDTH]; Ppu::DISPLAY_HEIGHT],
}

impl Ppu {
    const OAM_SIZE: usize = (OAM_END - OAM_START + 1) as usize;
    const VRAM_SIZE: usize = (VRAM_END - VRAM_START + 1) as usize;

    const DISPLAY_WIDTH: usize = 160;
    const DISPLAY_HEIGHT: usize = 144;

    pub fn new() -> Self {
        Self {
            ly: 0,
            lyc: 0,
            lx: 0,
            scx: 0,
            scy: 0,
            mode: PpuMode::OAM { ticks_left: 20 },
            vram: [0x00; Ppu::VRAM_SIZE],
            oam: [0x00; Ppu::OAM_SIZE],
            framebuffer: [[0x00000000; Ppu::DISPLAY_WIDTH]; Ppu::DISPLAY_HEIGHT],
        }
    }

    pub fn step(&mut self) -> Result<bool, Error> {
        match self.mode {
            PpuMode::OAM { ticks_left } => {
                if ticks_left == 0 {
                    // stat = 2
                    // check if LCDC Int is allowed
                    // call int
                    // forbid interr

                    self.mode = PpuMode::OAMRAM { ticks_left: 172 / 4 };
                } else {
                    self.mode = PpuMode::OAM { ticks_left: ticks_left - 1 };
                }
            }
            PpuMode::OAMRAM { ticks_left } => {
                if ticks_left == 0 {
                    self.mode = PpuMode::HBlank { ticks_left: 200 / 4 };
                } else {
                    self.mode = PpuMode::OAMRAM { ticks_left: ticks_left - 1 };
                }
            }
            PpuMode::HBlank { ticks_left } => {
                if ticks_left == 0 {
                    self.ly += 1;

                    if self.ly == 144 {
                        // Last visible line processed, entering VBlank
                        self.mode = PpuMode::VBlank { ticks_left: 0 };
                    } else {
                        self.mode = PpuMode::OAM { ticks_left: 80 };
                    }
                } else {
                    self.mode = PpuMode::HBlank { ticks_left: ticks_left - 1 };
                }
            }
            PpuMode::VBlank { ticks_left } => {
                if ticks_left == 0 {
                    self.ly += 1;

                    if self.ly == self.lyc {
                        // Call interrupt
                    }

                    if self.ly == 153 {
                        // Last blank line processed
                        self.ly = 0;
                        self.mode = PpuMode::VBlank { ticks_left: 452 / 4 - 1 };
                    } else {
                        self.mode = PpuMode::VBlank { ticks_left: 0 };
                    }
                } else {
                    self.mode = PpuMode::VBlank { ticks_left: ticks_left - 1 };
                }
            }
        }

        Ok(true)
    }

    pub fn fill_bitmap(&self, buffer: &mut [u32]) -> Result<(), Error> {
        let mut idx = 0;

        for line in self.framebuffer.iter() {
            for px in line.iter() {
                buffer[idx] = *px;
                idx += 1;
            }
        }

        Ok(())
    }
}

impl memory::Addressable for Ppu {
    fn address_ranges(&self) -> Vec<RangeInclusive<u16>> {
        vec![
            VRAM_START..=VRAM_END,
            OAM_START..=OAM_END,
            GPU_IO_START..=GPU_IO_END,
        ]
    }

    fn read(&self, address: u16) -> Result<u8, memory::Error> {
        match address {
            VRAM_START...VRAM_END => Ok(0),
            OAM_START...OAM_END => Ok(0),
            GPU_IO_START...GPU_IO_END => Ok(0),
            other => Err(memory::Error::Unavailable(other))
        }
    }

    fn write(&mut self, address: u16, byte: u8) -> Result<(), memory::Error> {
        match address {
            VRAM_START...VRAM_END => Ok(()),
            OAM_START...OAM_END => Ok(()),
            GPU_IO_START...GPU_IO_END => unimplemented!(),
            other => Err(memory::Error::Unavailable(other))
        }
    }
}
