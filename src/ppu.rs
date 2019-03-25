use std::cell::RefCell;
use std::io::Write;
use std::ops::RangeInclusive;
use std::rc::Rc;

use crate::interrupts::{Interrupt, InterruptController};
use crate::memory;
use crate::ppu::PPUMode::OAM;
use crate::util;
use crate::util::Bit;

pub enum Error {}

enum PPUMode {
    OAM { ticks_left: usize },
    OAMRAM { ticks_left: usize },
    HBlank { ticks_left: usize },
    VBlank { lines_left: usize, ticks_left: usize },
}

pub struct PPU {
    ly: u8,
    lyc: u8,
    lx: u8,
    scx: u8,
    scy: u8,
    lcdc: u8,
    stat: u8,
    dma: u8,
    bgp: u8,
    obp0: u8,
    obp1: u8,
    wy: u8,
    wx: u8,
    mode: PPUMode,
    vram: [u8; PPU::VRAM_SIZE],
    oam: [u8; PPU::OAM_SIZE],
    framebuffer: [[u32; PPU::DISPLAY_WIDTH]; PPU::DISPLAY_HEIGHT],
    memory: Rc<RefCell<dyn memory::Addressable>>,
}

impl PPU {
    const VRAM_START: u16 = 0x8000;
    const VRAM_END: u16 = 0x9FFF;
    const OAM_START: u16 = 0xFE00;
    const OAM_END: u16 = 0xFE9F;
    const GPU_IO_START: u16 = 0xFF40;
    const GPU_IO_END: u16 = 0xFF6B;

    #[doc = "LCD Control Register (R/W)"]
    const LCDC: u16 = 0xFF40;
    #[doc = "LCD Status (R/W)"]
    const STAT: u16 = 0xFF41;
    #[doc = "Scroll Y (R/W)"]
    const SCY: u16 = 0xFF42;
    #[doc = "Scroll X (R/W)"]
    const SCX: u16 = 0xFF43;
    #[doc = "LCDC Y-Coordinate (R)"]
    const LY: u16 = 0xFF44;
    #[doc = "LY Compare (R/W)"]
    const LYC: u16 = 0xFF45;
    #[doc = "DMA Transfer and Start Address (R/W)"]
    const DMA: u16 = 0xFF46;
    #[doc = "BG Palette Data (R/W)"]
    const BGP: u16 = 0xFF47;
    #[doc = "Object Palette 0 Data (R/W)"]
    const OBP0: u16 = 0xFF48;
    #[doc = "Object Palette 1 Data (R/W)"]
    const OBP1: u16 = 0xFF49;
    #[doc = "Window Y Position (R/W)"]
    const WY: u16 = 0xFF4A;
    #[doc = "Window X Position minus 7 (R/W)"]
    const WX: u16 = 0xFF4B;

    #[doc = "Bit 7 - LCD Display Enable (0=Off, 1=On)"]
    const DISPLAY_ENABLE: usize = 7;
    #[doc = "Bit 6 - Window Tile Map Display Select (0=9800-9BFF, 1=9C00-9FFF)"]
    const WIN_TILE_MAP: usize = 6;
    #[doc = "Bit 5 - Window Display Enable (0=Off, 1=On)"]
    const WINDOW_ENABLE: usize = 5;
    #[doc = "Bit 4 - BG & Window Tile Data Select (0=8800-97FF, 1=8000-8FFF)"]
    const TILE_DATA: usize = 4;
    #[doc = "Bit 3 - BG Tile Map Display Select (0=9800-9BFF, 1=9C00-9FFF)"]
    const BG_TILE_MAP: usize = 3;
    #[doc = "Bit 2 - OBJ (Sprite) Size (0=8x8, 1=8x16)"]
    const OBJ_SIZE: usize = 2;
    #[doc = "Bit 1 - OBJ (Sprite) Display Enable (0=Off, 1=On)"]
    const OBJ_ENABLE: usize = 1;
    #[doc = "Bit 0 - BG/Window Display/Priority (0=Off, 1=On)"]
    const BG_PRIORITY: usize = 0;

    #[doc = "Bit 6 - LYC=LY Coincidence Interrupt (1=Enable) (Read/Write)"]
    const LCY_INT: usize = 6;
    #[doc = "Bit 5 - Mode 2 OAM Interrupt (1=Enable) (Read/Write)"]
    const OAM_INT: usize = 5;
    #[doc = "Bit 4 - Mode 1 V-Blank Interrupt (1=Enable) (Read/Write)"]
    const VBLANK_INT: usize = 4;
    #[doc = "Bit 3 - Mode 0 H-Blank Interrupt (1=Enable) (Read/Write)"]
    const HBLANK_INT: usize = 3;
    #[doc = "Bit 2 - Coincidence Flag  (0:LYC<>LY, 1:LYC=LY) (Read Only)"]
    const LCY_FLAG: usize = 2;
    #[doc = "Bit 1-0 - Mode Flag (Mode 0-3, see below) (Read Only)"]
    const MODE_TRANSFER: usize = 1;
    const MODE: usize = 0;

    const OAM_SIZE: usize = (PPU::OAM_END - PPU::OAM_START + 1) as usize;
    const VRAM_SIZE: usize = (PPU::VRAM_END - PPU::VRAM_START + 1) as usize;

    const DISPLAY_WIDTH: usize = 160;
    const DISPLAY_HEIGHT: usize = 144;

    const VBLANK_LINE_TICKS: usize = 456 / 4;
    const VBLANK_LINES: usize = 456 / 4;

    const COLOR_0_RGB: u32 = 0xFFFFFF00;
    const COLOR_1_RGB: u32 = 0xAAAAAA00;
    const COLOR_2_RGB: u32 = 0x55555500;
    const COLOR_3_RGB: u32 = 0x00000000;

    pub fn new(memory: Rc<RefCell<dyn memory::Addressable>>) -> Self {
        Self {
            ly: 0,
            lyc: 0,
            lx: 0,
            scx: 0,
            scy: 0,
            lcdc: 0,
            stat: 0,
            dma: 0,
            bgp: 0,
            obp0: 0,
            obp1: 0,
            wy: 0,
            wx: 0,
            mode: PPUMode::OAM { ticks_left: 20 },
            vram: [0x00; PPU::VRAM_SIZE],
            oam: [0x00; PPU::OAM_SIZE],
            framebuffer: [[0x00000000; PPU::DISPLAY_WIDTH]; PPU::DISPLAY_HEIGHT],
            memory,
        }
    }

    pub fn step(&mut self) -> Result<bool, Error> {
        match self.mode {
            PPUMode::OAM { ticks_left } => {
                if ticks_left != 0 {
                    self.mode = PPUMode::OAM { ticks_left: ticks_left - 1 };
                } else {
                    self.mode = PPUMode::OAMRAM { ticks_left: 172 / 4 };
                }
            }
            PPUMode::OAMRAM { ticks_left } => {
                if ticks_left != 0 {
                    self.mode = PPUMode::OAMRAM { ticks_left: ticks_left - 1 };
                } else {
                    self.scanline();
                    self.mode = PPUMode::HBlank { ticks_left: 204 / 4 };
                }
            }
            PPUMode::HBlank { ticks_left } => {
                if ticks_left != 0 {
                    self.mode = PPUMode::HBlank { ticks_left: ticks_left - 1 };
                } else {
                    self.inc_ly();

                    if self.ly == PPU::DISPLAY_HEIGHT as u8 {
                        self.mode = PPUMode::VBlank { lines_left: PPU::VBLANK_LINES, ticks_left: PPU::VBLANK_LINE_TICKS };
                    } else {
                        self.mode = PPUMode::OAM { ticks_left: 80 };
                    }
                }
            }
            PPUMode::VBlank { lines_left, ticks_left } => {
                if ticks_left != 0 {
                    self.mode = PPUMode::VBlank { lines_left, ticks_left: ticks_left - 1 };
                } else {
                    if lines_left != 0 {
                        self.inc_ly();
                        self.mode = PPUMode::VBlank { lines_left: lines_left - 1, ticks_left: PPU::VBLANK_LINE_TICKS };
                    } else {
                        self.ly = 0;
                        self.mode = PPUMode::OAM { ticks_left: 20 };
                    }
                }
            }
        }

        self.set_mode();

        Ok(true)
    }

    fn set_mode(&mut self) {
        match self.mode {
            PPUMode::OAM { .. } => {
                // Mode 2
                self.stat.set_bit(PPU::MODE_TRANSFER, true as usize);
                self.stat.set_bit(PPU::MODE, false as usize);
            }
            PPUMode::OAMRAM { .. } => {
                // Mode 3
                self.stat.set_bit(PPU::MODE_TRANSFER, true as usize);
                self.stat.set_bit(PPU::MODE, true as usize);
            }
            PPUMode::HBlank { .. } => {
                // Mode 0
                self.stat.set_bit(PPU::MODE_TRANSFER, false as usize);
                self.stat.set_bit(PPU::MODE, false as usize);
            }
            PPUMode::VBlank { .. } => {
                // Mode 1
                self.stat.set_bit(PPU::MODE_TRANSFER, false as usize);
                self.stat.set_bit(PPU::MODE, true as usize);
            }
        }
    }

    fn scanline(&mut self) {
//        let mut line = self.framebuffer[self.ly];

        // VRAM offset for the tile map
//        let mut mapoffs = if util::get_bit(self.lcdc, PPU::BG_TILE_MAP) { 0x1C00 } else { 0x1800 };

        // Which line of tiles to use in the map
//        mapoffs += ((self.ly + self.scy) & 255) >> 3;

        // Which tile to start with in the map line
//        let lineoffs = self.scx >> 3;
//
//        // Which line of pixels to use in the tiles
//        var y = (GPU._line + GPU._scy) & 7;
//
//        // Where in the tileline to start
//        var x = GPU._scx & 7;
//
//        // Where to render on the canvas
//        var canvasoffs = GPU._line * 160 * 4;
//
//        // Read tile index from the background map
//        var colour;
//        var tile = GPU._vram[mapoffs + lineoffs];
//
//        // If the tile data set in use is #1, the
//        // indices are signed; calculate a real tile offset
//        if(GPU._bgtile == 1 && tile < 128) tile += 256;
//
//        for(var i = 0; i < 160; i++)
//        {
//            // Re-map the tile pixel through the palette
//            colour = GPU._pal[GPU._tileset[tile][y][x]];
//
//            // Plot the pixel to canvas
//            GPU._scrn.data[canvasoffs+0] = colour[0];
//            GPU._scrn.data[canvasoffs+1] = colour[1];
//            GPU._scrn.data[canvasoffs+2] = colour[2];
//            GPU._scrn.data[canvasoffs+3] = colour[3];
//            canvasoffs += 4;
//
//            // When this tile ends, read another
//            x++;
//            if(x == 8)
//            {
//                x = 0;
//                lineoffs = (lineoffs + 1) & 31;
//                tile = GPU._vram[mapoffs + lineoffs];
//                if(GPU._bgtile == 1 && tile < 128) tile += 256;
//            }
//        }
    }

    fn inc_ly(&mut self) {
        self.ly += 1;

        if self.ly == self.lyc {
            InterruptController::request(self.memory.borrow_mut(), Interrupt::LcdStat);
        }
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

impl memory::Addressable for PPU {
    fn address_ranges(&self) -> Vec<RangeInclusive<u16>> {
        vec![
            PPU::VRAM_START..=PPU::VRAM_END,
            PPU::OAM_START..=PPU::OAM_END,
            PPU::GPU_IO_START..=PPU::GPU_IO_END,
        ]
    }

    fn read(&self, address: u16) -> Result<u8, memory::Error> {
        match address {
            PPU::VRAM_START...PPU::VRAM_END => {
                match self.mode {
                    PPUMode::OAMRAM { .. } => Err(memory::Error::Unavailable(address)),
                    PPUMode::OAM { .. } | PPUMode::HBlank { .. } | PPUMode::VBlank { .. } => {
                        let addr = (address - PPU::VRAM_START) as usize;
                        Ok(self.vram[addr])
                    }
                }
            }
            PPU::OAM_START...PPU::OAM_END => {
                match self.mode {
                    PPUMode::OAM { .. } | PPUMode::OAMRAM { .. } => Err(memory::Error::Unavailable(address)),
                    PPUMode::HBlank { .. } | PPUMode::VBlank { .. } => {
                        let addr = (address - PPU::OAM_START) as usize;
                        Ok(self.oam[addr])
                    }
                }
            }
            PPU::LCDC => Ok(self.lcdc),
            PPU::STAT => Ok(self.stat),
            PPU::SCY => Ok(self.scy),
            PPU::SCX => Ok(self.scx),
            PPU::LY => Ok(self.ly),
            PPU::LYC => Ok(self.lyc),
            PPU::DMA => Ok(self.dma),
            PPU::BGP => Ok(self.bgp),
            PPU::OBP0 => Ok(self.obp0),
            PPU::OBP1 => Ok(self.obp1),
            PPU::WY => Ok(self.wy),
            PPU::WX => Ok(self.wx),
            other => Err(memory::Error::Unavailable(other))
        }
    }

    fn write(&mut self, address: u16, byte: u8) -> Result<(), memory::Error> {
        match address {
            PPU::VRAM_START...PPU::VRAM_END => {
                match self.mode {
                    PPUMode::OAMRAM { .. } => Err(memory::Error::Unavailable(address)),
                    PPUMode::OAM { .. } | PPUMode::HBlank { .. } | PPUMode::VBlank { .. } => {
                        let addr = (address - PPU::VRAM_START) as usize;
                        Ok(self.vram[addr] = byte)
                    }
                }
            }
            PPU::OAM_START...PPU::OAM_END => {
                match self.mode {
                    PPUMode::OAM { .. } | PPUMode::OAMRAM { .. } => Err(memory::Error::Unavailable(address)),
                    PPUMode::HBlank { .. } | PPUMode::VBlank { .. } => {
                        let addr = (address - PPU::OAM_START) as usize;
                        Ok(self.oam[addr] = byte)
                    }
                }
            }
            PPU::LCDC => Ok(self.lcdc = byte),
            PPU::STAT => Ok(self.stat = byte),
            PPU::SCY => Ok(self.scy = byte),
            PPU::SCX => Ok(self.scx = byte),
            PPU::LY => Ok(self.ly = byte),
            PPU::LYC => Ok(self.lyc = byte),
            PPU::DMA => Ok(self.dma = byte),
            PPU::BGP => Ok(self.bgp = byte),
            PPU::OBP0 => Ok(self.obp0 = byte),
            PPU::OBP1 => Ok(self.obp1 = byte),
            PPU::WY => Ok(self.wy = byte),
            PPU::WX => Ok(self.wx = byte),
            other => Err(memory::Error::Unavailable(other))
        }
    }
}
