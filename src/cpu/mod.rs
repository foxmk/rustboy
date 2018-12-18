// mod.rs

use std;
use std::fmt;
use std::cell::RefCell;
use std::rc::Rc;
use std::rc::Weak;

use crate::util;
use crate::memory;

const FLAG_IE: u16 = 0xFFFF;
const FLAG_IF: u16 = 0xFF0F;

const INT_VBLANK: u8 = 0;
const INT_LCD_STAT: u8 = 1;
const INT_TIMER: u8 = 2;
const INT_SERIAL: u8 = 3;
const INT_JOYSTICK: u8 = 4;

const INT_VBLANK_ADDR: u16 = 0x0040;
const INT_LCD_STAT_ADDR: u16 = 0x0040;
const INT_TIMER_ADDR: u16 = 0x0040;
const INT_SERIAL_ADDR: u16 = 0x0040;
const INT_JOYSTICK_ADDR: u16 = 0x0040;

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    UnknownOpCode,
    InvalidMemoryAccess,
}

#[derive(Debug)]
enum Reg8 { A, B, C, D, E, H, L }

#[derive(Debug)]
enum Reg16 { AF, BC, DE, HL, SP, PC }

#[derive(Debug)]
enum Flag { Z, N, H, C }

mod op;

use crate::cpu::op::*;

pub struct Cpu {
    pc: u16,
    sp: u16,
    ticks: u32,
    registers: [u16; 4],
    interrupt_enable_master: bool,
    memory: Rc<RefCell<memory::Addressable>>,
}

impl Cpu {
    pub fn new(memory: Rc<RefCell<memory::Addressable>>) -> Self {
        Self {
            pc: 0,
            sp: 0,
            ticks: 0,
            registers: [0; 4],
            interrupt_enable_master: false,
            memory: memory.clone(),
        }
    }

    pub fn step(&mut self) -> Result<bool, Error> {
        let mut buf = vec![];

        let op: Op = loop {
            buf.push(self.fetch()?);

            match Op::decode(&buf) {
                Ok(Some(op)) => break op,
                Ok(None) => continue,
                Err(err) => return Err(err),
            }
        };

        #[cfg(test)]
            {
                println!("Exec op: {}", op)
            }

        op.exec(self)
//        self.process_interrupts()
    }

    fn fetch(&mut self) -> Result<u8, Error> {
        let byte = match self.memory.borrow().get(self.pc) {
            Ok(byte) => byte,
            Err(_) => return Err(Error::InvalidMemoryAccess)
        };

        self.pc += 1;
        return Ok(byte);
    }

    fn process_interrupts(&mut self) -> Result<bool, Error> {
        if self.interrupt_enable_master {
            let interrupt_enable_flags = match self.memory.borrow().get(FLAG_IE) {
                Ok(byte) => byte,
                Err(_) => return Err(Error::InvalidMemoryAccess)
            };

            let interrupt_flags = match self.memory.borrow().get(FLAG_IF) {
                Ok(byte) => byte,
                Err(_) => return Err(Error::InvalidMemoryAccess)
            };

            match self.process_interrupt_if_requested_and_allowed(interrupt_enable_flags, interrupt_flags, INT_VBLANK, INT_VBLANK_ADDR) {
                Ok(false) => (),
                other => return other,
            }
            match self.process_interrupt_if_requested_and_allowed(interrupt_enable_flags, interrupt_flags, INT_LCD_STAT, INT_LCD_STAT_ADDR) {
                Ok(false) => (),
                other => return other,
            }
            match self.process_interrupt_if_requested_and_allowed(interrupt_enable_flags, interrupt_flags, INT_TIMER, INT_TIMER_ADDR) {
                Ok(false) => (),
                other => return other,
            }
            match self.process_interrupt_if_requested_and_allowed(interrupt_enable_flags, interrupt_flags, INT_SERIAL, INT_SERIAL_ADDR) {
                Ok(false) => (),
                other => return other,
            }
            match self.process_interrupt_if_requested_and_allowed(interrupt_enable_flags, interrupt_flags, INT_JOYSTICK, INT_JOYSTICK_ADDR) {
                Ok(false) => (),
                other => return other,
            }
        }

        Ok(true)
    }

    fn process_interrupt_if_requested_and_allowed(&mut self, interrupt_enable_flags: u8, interrupt_flags: u8, interrupt: u8, interrupt_addr: u16) -> Result<bool, Error> {
        if util::get_bit(interrupt_enable_flags, interrupt) && util::get_bit(interrupt_flags, interrupt) {
            self.interrupt_enable_master = false;

            let mut flags_to_save = interrupt_flags.clone();
            util::set_bit(&mut flags_to_save, interrupt, false);

            let mut memory = self.memory.borrow_mut();
            match memory.set(FLAG_IF, flags_to_save) {
                Ok(_) => (),
                Err(_) => return Err(Error::InvalidMemoryAccess)
            }

            self.sp -= 2;
            memory.set(self.sp + 1, util::get_high_byte(self.pc));
            memory.set(self.sp, util::get_low_byte(self.pc));
            self.pc = interrupt_addr;
            return Ok(true);
        }

        Ok(false)
    }

    fn get_byte_register(&self, reg: &Reg8) -> u8 {
        match reg {
            Reg8::A => util::get_high_byte(self.registers[0]),
            Reg8::B => util::get_high_byte(self.registers[1]),
            Reg8::C => util::get_low_byte(self.registers[1]),
            Reg8::D => util::get_high_byte(self.registers[2]),
            Reg8::E => util::get_low_byte(self.registers[2]),
            Reg8::H => util::get_high_byte(self.registers[3]),
            Reg8::L => util::get_low_byte(self.registers[3]),
        }
    }

    fn set_byte_register(&mut self, reg: &Reg8, byte: u8) {
        match reg {
            Reg8::A => util::set_high_byte(&mut self.registers[0], byte),
            Reg8::B => util::set_high_byte(&mut self.registers[1], byte),
            Reg8::C => util::set_low_byte(&mut self.registers[1], byte),
            Reg8::D => util::set_high_byte(&mut self.registers[2], byte),
            Reg8::E => util::set_low_byte(&mut self.registers[2], byte),
            Reg8::H => util::set_high_byte(&mut self.registers[3], byte),
            Reg8::L => util::set_low_byte(&mut self.registers[3], byte),
        }
    }

    fn get_word_register(&self, reg: &Reg16) -> u16 {
        match reg {
            Reg16::AF => self.registers[0],
            Reg16::BC => self.registers[1],
            Reg16::DE => self.registers[2],
            Reg16::HL => self.registers[3],
            Reg16::SP => self.sp,
            Reg16::PC => self.pc,
        }
    }

    fn set_word_register(&mut self, reg: &Reg16, word: u16) {
        match reg {
            Reg16::AF => self.registers[0] = word,
            Reg16::BC => self.registers[1] = word,
            Reg16::DE => self.registers[2] = word,
            Reg16::HL => self.registers[3] = word,
            Reg16::SP => self.sp = word,
            Reg16::PC => self.pc = word,
        }
    }

    fn get_flag(&self, flag: Flag) -> bool {
        match flag {
            Flag::C => util::get_bit(util::get_low_byte(self.registers[0]), 0),
            Flag::H => util::get_bit(util::get_low_byte(self.registers[0]), 0),
            Flag::N => util::get_bit(util::get_low_byte(self.registers[0]), 0),
            Flag::Z => util::get_bit(util::get_low_byte(self.registers[0]), 0),
        }
    }

    fn set_flag(&mut self, flag: Flag, value: bool) {
        match flag {
            Flag::C => {
                let mut byte = util::get_low_byte(self.registers[0]);
                util::set_bit(&mut byte, 0, value);
                util::set_high_byte(&mut self.registers[0], byte);
            }
            Flag::H => {
                let mut byte = util::get_low_byte(self.registers[0]);
                util::set_bit(&mut byte, 0, value);
                util::set_high_byte(&mut self.registers[0], byte);
            }
            Flag::N => {
                let mut byte = util::get_low_byte(self.registers[0]);
                util::set_bit(&mut byte, 0, value);
                util::set_high_byte(&mut self.registers[0], byte);
            }
            Flag::Z => {
                let mut byte = util::get_low_byte(self.registers[0]);
                util::set_bit(&mut byte, 0, value);
                util::set_high_byte(&mut self.registers[0], byte);
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use std::ops::RangeInclusive;

    use crate::memory;
    use crate::memory::Addressable;
    use crate::memory::test_util::VecMemory;

    #[test]
    fn should_increment_pc() {
        let memory = VecMemory::new_ref(compile(vec![
            Op::Nop,
            Op::Halt,
        ]));

        let mut cpu = Cpu::new(memory.clone());
        assert_eq!(cpu.pc, 0, "PC should be 0");

        cpu.step();
        assert_eq!(cpu.pc, 1, "PC should be 1");
    }

    #[test]
    fn simple_addition() {
        let memory = VecMemory::new_ref_ranged(compile(vec![
            Op::LoadImm(Reg8::A, 0x01),
            Op::LoadImm(Reg8::B, 0x03),
            Op::Load16(Reg16::HL, 0x00FF),
            Op::Add(Reg8::B),
            Op::StoreInd(Reg16::HL, Reg8::A),
            Op::Halt,
        ]), 0x0000..=0x0100);

        let mut cpu = Cpu::new(memory.clone());
        loop {
            match cpu.step() {
                Ok(true) => continue,
                Ok(false) => break,
                Err(err) => panic!("{:?}", err)
            }
        }

        assert_eq!(memory.borrow().get(0x00FF), Ok(0x01 + 0x03));
    }

    #[test]
    fn interrupts() {}
}
