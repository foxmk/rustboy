use core::borrow::BorrowMut;
use std::cell::{Ref, RefMut};
use std::fmt;
use std::ops::RangeInclusive;

use memory::Addressable;

use crate::{memory, util};
use crate::interrupts::Interrupt::Serial;
use crate::util::Bit;

#[derive(Debug)]
pub enum Interrupt {
    VBlank,
    LcdStat,
    Timer,
    Serial,
    Joystick,
}

impl Interrupt {
    pub fn address(&self) -> u16 {
        match self {
            Interrupt::VBlank => 0x0040,
            Interrupt::LcdStat => 0x0048,
            Interrupt::Timer => 0x0050,
            Interrupt::Serial => 0x0058,
            Interrupt::Joystick => 0x0060,
        }
    }
}

impl fmt::Display for Interrupt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.address())
    }
}

pub struct InterruptController {
    enable_flags: u8,
    request_flags: u8,
}

impl InterruptController {
    const FLAG_IE: u16 = 0xFFFF;
    const FLAG_IF: u16 = 0xFF0F;

    pub fn new() -> Self {
        Self {
            enable_flags: 0b0000000,
            request_flags: 0b00000000,
        }
    }

    pub fn poll(mem: Ref<dyn Addressable>) -> Result<Option<Interrupt>, memory::Error> {
        let enable_flags = mem.read(InterruptController::FLAG_IE)?;
        let request_flags = mem.read(InterruptController::FLAG_IF)?;

        match InterruptController::get_leftmost_set_bit(enable_flags & request_flags) {
            0 => Ok(Some(Interrupt::VBlank)),
            1 => Ok(Some(Interrupt::LcdStat)),
            2 => Ok(Some(Interrupt::Timer)),
            3 => Ok(Some(Interrupt::Serial)),
            4 => Ok(Some(Interrupt::Joystick)),
            _ => Ok(None)
        }
    }

    pub fn request(mut mem: RefMut<dyn Addressable>, int: Interrupt) -> Result<(), memory::Error> {
        let mut flags_to_save = mem.read(InterruptController::FLAG_IF)?;

        match int {
            Interrupt::VBlank => flags_to_save.set_bit(0, 1),
            Interrupt::LcdStat => flags_to_save.set_bit(1, 1),
            Interrupt::Timer => flags_to_save.set_bit(2, 1),
            Interrupt::Serial => flags_to_save.set_bit(3, 1),
            Interrupt::Joystick => flags_to_save.set_bit(4, 1),
        };

        mem.write(InterruptController::FLAG_IF, flags_to_save)
    }

    pub fn clear_request(mut mem: RefMut<dyn Addressable>, int: Interrupt) -> Result<(), memory::Error> {
        let mut flags_to_save = mem.read(InterruptController::FLAG_IF)?;

        match int {
            Interrupt::VBlank => flags_to_save.set_bit(0, 0),
            Interrupt::LcdStat => flags_to_save.set_bit(1, 0),
            Interrupt::Timer => flags_to_save.set_bit(2, 0),
            Interrupt::Serial => flags_to_save.set_bit(3, 0),
            Interrupt::Joystick => flags_to_save.set_bit(4, 0),
        };

        mem.write(InterruptController::FLAG_IF, flags_to_save)
    }


    #[inline]
    fn get_leftmost_set_bit(byte: u8) -> usize {
//        ((byte as i8) & -(byte as i8)) as i64.log2 + 1
        0
    }
}

impl Addressable for InterruptController {
    fn address_ranges(&self) -> Vec<RangeInclusive<u16>> {
        vec![
            InterruptController::FLAG_IE..=InterruptController::FLAG_IE,
            InterruptController::FLAG_IF..=InterruptController::FLAG_IF,
        ]
    }

    fn read(&self, address: u16) -> Result<u8, memory::Error> {
        match address {
            InterruptController::FLAG_IE => Ok(self.enable_flags),
            InterruptController::FLAG_IF => Ok(self.request_flags),
            other => Err(memory::Error::Unavailable(other))
        }
    }

    fn write(&mut self, address: u16, byte: u8) -> Result<(), memory::Error> {
        match address {
            InterruptController::FLAG_IE => {
                Ok(self.enable_flags = byte)
            }
            InterruptController::FLAG_IF => {
                Ok(self.request_flags = byte)
            }
            other => Err(memory::Error::Unavailable(other))
        }
    }
}