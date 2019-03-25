use memory::Addressable;
use std::fmt;
use std::ops::RangeInclusive;
use crate::{memory, util};
use crate::interrupts::Interrupt::Serial;
use std::cell::{Ref, RefMut};
use core::borrow::BorrowMut;

#[derive(Debug)]
pub enum Interrupt {
    VBlank,
    LcdStat,
    Timer,
    Serial,
    Joystick,
    // Fake interrupts
    Int0x00,
    Int0x08,
    Int0x10,
    Int0x18,
    Int0x20,
    Int0x28,
    Int0x30,
    Int0x38,
}

impl Interrupt {
    pub fn address(&self) -> u16 {
        match self {
            Interrupt::Int0x00 => 0x0000,
            Interrupt::Int0x08 => 0x0008,
            Interrupt::Int0x10 => 0x0010,
            Interrupt::Int0x18 => 0x0018,
            Interrupt::Int0x20 => 0x0020,
            Interrupt::Int0x28 => 0x0028,
            Interrupt::Int0x30 => 0x0030,
            Interrupt::Int0x38 => 0x0038,
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
//        let request_flags = self.mem_read(FLAG_IF)?

//        match self {
//            Interrupt::VBlank => 0,
//            Interrupt::LcdStat => 1,
//            Interrupt::Timer => 2,
//            Interrupt::Serial => 3,
//            Interrupt::Joystick => 4,
//        }
        Ok(None)
    }

    pub fn request(mem: RefMut<dyn Addressable>, int: Interrupt) -> Result<(), memory::Error> {
        Ok(())
    }

    pub fn clear_request(mut mem: RefMut<dyn Addressable>, int: Interrupt) -> Result<(), memory::Error> {
        let mut flags_to_save = mem.read(InterruptController::FLAG_IF)?;

        match int {
            Interrupt::VBlank => util::set_bit(&mut flags_to_save, 0, false),
            Interrupt::LcdStat => util::set_bit(&mut flags_to_save, 1, false),
            Interrupt::Timer => util::set_bit(&mut flags_to_save, 2, false),
            Interrupt::Serial => util::set_bit(&mut flags_to_save, 3, false),
            Interrupt::Joystick => util::set_bit(&mut flags_to_save, 4, false),
            _ => unimplemented!()
        }

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