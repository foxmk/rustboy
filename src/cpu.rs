use std;
use std::cell::{Cell, RefCell};
use std::collections::VecDeque;
use std::fmt;
use std::fmt::Debug;
use std::io::{Read, Write};
use std::io::SeekFrom::Start;
use std::ops::{Index, IndexMut, RangeInclusive};
use std::rc::Rc;
use std::rc::Weak;

use crate::cpu::MicroOp::FetchAndDecode;
use crate::interrupts::InterruptController;
use crate::memory;
use crate::util;
use crate::util::{Byte, Bit};

#[derive(Debug, Clone, Copy)]
pub(crate) enum Reg8 { A, B, C, D, E, H, L, SPHi, SPLow, PCHi, PCLo, TempHi, TempLow }

#[derive(Debug)]
pub(crate) enum Reg16 { AF, BC, DE, HL, SP, PC, Temp }

#[derive(Debug)]
pub(crate) enum Flag { Z, N, H, C }

#[derive(Debug)]
pub(crate) enum RstVector {
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

impl RstVector {
    pub fn address(&self) -> u16 {
        match self {
            RstVector::Int0x00 => 0x0000,
            RstVector::Int0x08 => 0x0008,
            RstVector::Int0x10 => 0x0010,
            RstVector::Int0x18 => 0x0018,
            RstVector::Int0x20 => 0x0020,
            RstVector::Int0x28 => 0x0028,
            RstVector::Int0x30 => 0x0030,
            RstVector::Int0x38 => 0x0038,
        }
    }
}

impl fmt::Display for RstVector {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.address())
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    UnknownOpCode,
    InvalidMemoryAccess,
}

impl From<memory::Error> for Error {
    fn from(err: memory::Error) -> Self {
        match err {
            memory::Error::Unavailable(_) => Error::InvalidMemoryAccess,
            memory::Error::ReadOnly(_) => Error::InvalidMemoryAccess,
        }
    }
}

#[derive(Debug)]
enum AddressMode {
    Immediate,
    Indirect(Reg16),
    IndirectIO(Reg8),
    IndirectInc,
    IndirectDec,
}

enum PostOp {
    Add(Reg8),
    AddCarry(Reg8),
    Sub(Reg8),
    SubCarry(Reg8),
    And(Reg8),
    Xor(Reg8),
    Or(Reg8),
    Cmp(Reg8),
}

enum MicroOp {
    FetchAndDecode,
    IrqEnablePoll,
    Irq { enable_flags: u8 },
    FetchPrefix,
    MemRead(AddressMode, Reg8, Option<PostOp>),
    MemWrite(AddressMode, Reg8),
}

pub struct CPU {
    halted: bool,
    pc: u16,
    sp: u16,
    temp_reg: u16,
    registers: [u16; 4],
    memory: Rc<RefCell<dyn memory::Addressable>>,
    op_queue: VecDeque<MicroOp>,
    interrupt_enable_master: bool,
}

impl CPU {
    pub fn new(memory: Rc<RefCell<dyn memory::Addressable>>) -> Self {
        let mut op_queue = VecDeque::with_capacity(3);
        op_queue.push_back(MicroOp::FetchAndDecode);

        Self {
            halted: false,
            pc: 0,
            sp: 0,
            temp_reg: 0,
            registers: [0; 4],
            memory,
            op_queue,
            interrupt_enable_master: false,
        }
    }

    pub fn step(&mut self) -> Result<bool, Error> {
        match self.dequeue() {
            MicroOp::IrqEnablePoll => {
                // Dummy OP: interrupt request check does 2 memory accesses, thus 2 ticks
            }
            MicroOp::Irq { enable_flags } => {
                match InterruptController::poll(self.memory.borrow())? {
                    Some(interrupt) => {
                        self.interrupt_enable_master = false;

                        {
                            InterruptController::clear_request(self.memory.borrow_mut(), interrupt)?;
                        }

                        // Call iaddr
//                        self.sp -= 2;
//                        self.enqueue(MicroOp::MemWriteInd { addr_reg: Reg16::SP, reg: Reg8::A });
//                        self.memory.borrow_mut().write(self.sp + 1, util::get_high_byte(self.pc));
//                        self.memory.borrow_mut().write(self.sp, util::get_low_byte(self.pc));
//                        self.pc = interrupt.address();
//                        Reg8::PC_HI => util::get_high_byte(self.pc),
//                        Reg8::PC_LO => util::get_low_byte(self.pc),
//                        Reg8::SP_HI => util::get_high_byte(self.sp),
//                        Reg8::SP_LO => util::get_low_byte(self.sp),
//                        Reg8::PC_HI => util::set_high_byte(&mut self.pc, byte),
//                        Reg8::PC_LO => util::set_low_byte(&mut self.pc, byte),
//                        Reg8::SP_HI => util::set_high_byte(&mut self.sp, byte),
//                        Reg8::SP_LO => util::set_low_byte(&mut self.sp, byte),
                    }
                    None => self.enqueue(MicroOp::FetchAndDecode)
                }
            }
            MicroOp::FetchAndDecode => {
                let addr = self.read_and_inc_pc();
                let opcode = self.mem_read(addr)?;
                self.decode(opcode)?;
            }
            MicroOp::FetchPrefix => {
                let addr = self.read_and_inc_pc();
                let opcode = self.mem_read(addr)?;
                self.decode_prefix(opcode)?;
            }
            MicroOp::MemRead(address_mode, dst, post_op) => {
                let addr = self.get_addr(address_mode);
                let byte = self.mem_read(addr)?;
                self.reg_write_byte(dst, byte);

                match post_op {
                    Some(op) => {},
                    None => {}
                };
            }
            MicroOp::MemWrite(address_mode, src) => {
                let addr = self.get_addr(address_mode);
                let byte = self.reg_read_byte(src);
                self.mem_write(addr, byte)?;
            }
        }
        Ok(!self.halted)
    }

    #[inline]
    fn decode(&mut self, opcode: u8) -> Result<(), Error> {
        match dbg!(opcode) {
            0xCB => self.enqueue(MicroOp::FetchPrefix),
            0x7F => self.load_reg(Reg8::A, Reg8::A),
            0x78 => self.load_reg(Reg8::A, Reg8::B),
            0x79 => self.load_reg(Reg8::A, Reg8::C),
            0x7A => self.load_reg(Reg8::A, Reg8::D),
            0x7B => self.load_reg(Reg8::A, Reg8::E),
            0x7C => self.load_reg(Reg8::A, Reg8::H),
            0x7D => self.load_reg(Reg8::A, Reg8::L),
            0x47 => self.load_reg(Reg8::B, Reg8::A),
            0x40 => self.load_reg(Reg8::B, Reg8::B),
            0x41 => self.load_reg(Reg8::B, Reg8::C),
            0x42 => self.load_reg(Reg8::B, Reg8::D),
            0x43 => self.load_reg(Reg8::B, Reg8::E),
            0x44 => self.load_reg(Reg8::B, Reg8::H),
            0x45 => self.load_reg(Reg8::B, Reg8::L),
            0x4F => self.load_reg(Reg8::C, Reg8::A),
            0x48 => self.load_reg(Reg8::C, Reg8::B),
            0x49 => self.load_reg(Reg8::C, Reg8::C),
            0x4A => self.load_reg(Reg8::C, Reg8::D),
            0x4B => self.load_reg(Reg8::C, Reg8::E),
            0x4C => self.load_reg(Reg8::C, Reg8::H),
            0x4D => self.load_reg(Reg8::C, Reg8::L),
            0x57 => self.load_reg(Reg8::D, Reg8::A),
            0x50 => self.load_reg(Reg8::D, Reg8::B),
            0x51 => self.load_reg(Reg8::D, Reg8::C),
            0x52 => self.load_reg(Reg8::D, Reg8::D),
            0x53 => self.load_reg(Reg8::D, Reg8::E),
            0x54 => self.load_reg(Reg8::D, Reg8::H),
            0x55 => self.load_reg(Reg8::D, Reg8::L),
            0x5F => self.load_reg(Reg8::E, Reg8::A),
            0x58 => self.load_reg(Reg8::E, Reg8::B),
            0x59 => self.load_reg(Reg8::E, Reg8::C),
            0x5A => self.load_reg(Reg8::E, Reg8::D),
            0x5B => self.load_reg(Reg8::E, Reg8::E),
            0x5C => self.load_reg(Reg8::E, Reg8::H),
            0x5D => self.load_reg(Reg8::E, Reg8::L),
            0x67 => self.load_reg(Reg8::H, Reg8::A),
            0x60 => self.load_reg(Reg8::H, Reg8::B),
            0x61 => self.load_reg(Reg8::H, Reg8::C),
            0x62 => self.load_reg(Reg8::H, Reg8::D),
            0x63 => self.load_reg(Reg8::H, Reg8::E),
            0x64 => self.load_reg(Reg8::H, Reg8::H),
            0x65 => self.load_reg(Reg8::H, Reg8::L),
            0x6F => self.load_reg(Reg8::L, Reg8::A),
            0x68 => self.load_reg(Reg8::L, Reg8::B),
            0x69 => self.load_reg(Reg8::L, Reg8::C),
            0x6A => self.load_reg(Reg8::L, Reg8::D),
            0x6B => self.load_reg(Reg8::L, Reg8::E),
            0x6C => self.load_reg(Reg8::L, Reg8::H),
            0x6D => self.load_reg(Reg8::L, Reg8::L),
            0x3E => self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::A, None)),
            0x06 => self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::B, None)),
            0x0E => self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::C, None)),
            0x16 => self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::D, None)),
            0x1E => self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::E, None)),
            0x26 => self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::H, None)),
            0x2E => self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::L, None)),
            0x0A => self.enqueue(MicroOp::MemRead(AddressMode::Indirect(Reg16::BC), Reg8::A, None)),
            0x1A => self.enqueue(MicroOp::MemRead(AddressMode::Indirect(Reg16::DE), Reg8::A, None)),
            0x7E => self.enqueue(MicroOp::MemRead(AddressMode::Indirect(Reg16::HL), Reg8::A, None)),
            0x46 => self.enqueue(MicroOp::MemRead(AddressMode::Indirect(Reg16::HL), Reg8::B, None)),
            0x4E => self.enqueue(MicroOp::MemRead(AddressMode::Indirect(Reg16::HL), Reg8::C, None)),
            0x56 => self.enqueue(MicroOp::MemRead(AddressMode::Indirect(Reg16::HL), Reg8::D, None)),
            0x5E => self.enqueue(MicroOp::MemRead(AddressMode::Indirect(Reg16::HL), Reg8::E, None)),
            0x66 => self.enqueue(MicroOp::MemRead(AddressMode::Indirect(Reg16::HL), Reg8::H, None)),
            0x6E => self.enqueue(MicroOp::MemRead(AddressMode::Indirect(Reg16::HL), Reg8::L, None)),
            0xFA => {
                self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::TempHi, None));
                self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::TempLow, None));
                self.enqueue(MicroOp::MemRead(AddressMode::Indirect(Reg16::Temp), Reg8::A, None));
            }
            0x02 => self.enqueue(MicroOp::MemWrite(AddressMode::Indirect(Reg16::BC), Reg8::A)),
            0x12 => self.enqueue(MicroOp::MemWrite(AddressMode::Indirect(Reg16::DE), Reg8::A)),
            0x77 => self.enqueue(MicroOp::MemWrite(AddressMode::Indirect(Reg16::HL), Reg8::A)),
            0x70 => self.enqueue(MicroOp::MemWrite(AddressMode::Indirect(Reg16::HL), Reg8::B)),
            0x71 => self.enqueue(MicroOp::MemWrite(AddressMode::Indirect(Reg16::HL), Reg8::C)),
            0x72 => self.enqueue(MicroOp::MemWrite(AddressMode::Indirect(Reg16::HL), Reg8::D)),
            0x73 => self.enqueue(MicroOp::MemWrite(AddressMode::Indirect(Reg16::HL), Reg8::E)),
            0x74 => self.enqueue(MicroOp::MemWrite(AddressMode::Indirect(Reg16::HL), Reg8::H)),
            0x75 => self.enqueue(MicroOp::MemWrite(AddressMode::Indirect(Reg16::HL), Reg8::L)),
            0x36 => {
                self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::TempHi, None));
                self.enqueue(MicroOp::MemWrite(AddressMode::Indirect(Reg16::HL), Reg8::TempHi));
            }
            0xEA => {
                self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::TempHi, None));
                self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::TempLow, None));
                self.enqueue(MicroOp::MemWrite(AddressMode::Indirect(Reg16::Temp), Reg8::A));
            }
            0xF0 => {
                self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::TempHi, None));
                self.enqueue(MicroOp::MemRead(AddressMode::IndirectIO(Reg8::TempHi), Reg8::A, None));
            }
            0xE0 => {
                self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::TempHi, None));
                self.enqueue(MicroOp::MemWrite(AddressMode::IndirectIO(Reg8::TempHi), Reg8::A));
            }
            0xF2 => self.enqueue(MicroOp::MemRead(AddressMode::IndirectIO(Reg8::C), Reg8::A, None)),
            0xE2 => self.enqueue(MicroOp::MemWrite(AddressMode::IndirectIO(Reg8::C), Reg8::A)),
//            0x2A => self.enqueue(MicroOp::MemRead(AddressMode::Indirect(Reg16::HL), Reg8::A, Box::new(|cpu| { CPU::reg_write_word(cpu, Reg16::HL, CPU::reg_read_word(cpu, Reg16::HL) + 1) }))),
//            0x22 => self.enqueue(MicroOp::MemWriteAndDo(AddressMode::Indirect(Reg16::HL), Reg8::A, Box::new(|cpu| { CPU::reg_write_word(cpu, Reg16::HL, CPU::reg_read_word(cpu, Reg16::HL) + 1) }))),
//            0x3A => self.enqueue(MicroOp::MemRead(AddressMode::Indirect(Reg16::HL), Reg8::A, Box::new(|cpu| { CPU::reg_write_word(cpu, Reg16::HL, CPU::reg_read_word(cpu, Reg16::HL) - 1) }))),
//            0x32 => self.enqueue(MicroOp::MemWriteAndDo(AddressMode::Indirect(Reg16::HL), Reg8::A, Box::new(|cpu| { CPU::reg_write_word(cpu, Reg16::HL, CPU::reg_read_word(cpu, Reg16::HL) - 1) }))),
            0x01 => {
                self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::B, None));
                self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::C, None));
            }
            0x11 => {
                self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::D, None));
                self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::E, None));
            }
            0x21 => {
                self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::H, None));
                self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::L, None));
            }
            0x31 => {
                self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::SPHi, None));
                self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::SPLow, None));
            }
////                [0xF9] => Ok(Some(Op::LoadSP)),
////                [0xC5] => Ok(Some(Op::Push(Reg16::BC))),
////                [0xD5] => Ok(Some(Op::Push(Reg16::DE))),
////                [0xE5] => Ok(Some(Op::Push(Reg16::HL))),
////                [0xF5] => Ok(Some(Op::Push(Reg16::AF))),
////                [0xC1] => Ok(Some(Op::Pop(Reg16::BC))),
////                [0xD1] => Ok(Some(Op::Pop(Reg16::DE))),
////                [0xE1] => Ok(Some(Op::Pop(Reg16::HL))),
////                [0xF1] => Ok(Some(Op::Pop(Reg16::AF))),
            0x87 => self.add_byte_reg(Reg8::A),
            0x80 => self.add_byte_reg(Reg8::B),
            0x81 => self.add_byte_reg(Reg8::C),
            0x82 => self.add_byte_reg(Reg8::D),
            0x83 => self.add_byte_reg(Reg8::E),
            0x84 => self.add_byte_reg(Reg8::H),
            0x85 => self.add_byte_reg(Reg8::L),
            0xC6 => self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::A, Some(PostOp::Add(Reg8::A)))),
            0x86 => self.enqueue(MicroOp::MemRead(AddressMode::Indirect(Reg16::HL), Reg8::TempHi, Some(PostOp::Add(Reg8::TempHi)))),
            0x8F => self.add_carry_byte_reg(Reg8::A),
            0x88 => self.add_carry_byte_reg(Reg8::B),
            0x89 => self.add_carry_byte_reg(Reg8::C),
            0x8A => self.add_carry_byte_reg(Reg8::D),
            0x8B => self.add_carry_byte_reg(Reg8::E),
            0x8C => self.add_carry_byte_reg(Reg8::H),
            0x8D => self.add_carry_byte_reg(Reg8::L),
            0xCE => self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::A, Some(PostOp::AddCarry(Reg8::A)))),
            0x8E => self.enqueue(MicroOp::MemRead(AddressMode::Indirect(Reg16::HL), Reg8::TempHi, Some(PostOp::AddCarry(Reg8::TempHi)))),
            0x97 => self.sub_byte_reg(Reg8::A),
            0x90 => self.sub_byte_reg(Reg8::B),
            0x91 => self.sub_byte_reg(Reg8::C),
            0x92 => self.sub_byte_reg(Reg8::D),
            0x93 => self.sub_byte_reg(Reg8::E),
            0x94 => self.sub_byte_reg(Reg8::H),
            0x95 => self.sub_byte_reg(Reg8::L),
            0xD6 => self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::A, Some(PostOp::Sub(Reg8::A)))),
            0x96 => self.enqueue(MicroOp::MemRead(AddressMode::Indirect(Reg16::HL), Reg8::TempHi, Some(PostOp::Sub(Reg8::TempHi)))),
            0x9F => self.sub_carry_byte_reg(Reg8::A),
            0x98 => self.sub_carry_byte_reg(Reg8::B),
            0x99 => self.sub_carry_byte_reg(Reg8::C),
            0x9A => self.sub_carry_byte_reg(Reg8::D),
            0x9B => self.sub_carry_byte_reg(Reg8::E),
            0x9C => self.sub_carry_byte_reg(Reg8::H),
            0x9D => self.sub_carry_byte_reg(Reg8::L),
            0xDE => self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::A, Some(PostOp::SubCarry(Reg8::A)))),
            0x9E => self.enqueue(MicroOp::MemRead(AddressMode::Indirect(Reg16::HL), Reg8::TempHi, Some(PostOp::SubCarry(Reg8::TempHi)))),
            0xA7 => self.and_byte_reg(Reg8::A),
            0xA0 => self.and_byte_reg(Reg8::B),
            0xA1 => self.and_byte_reg(Reg8::C),
            0xA2 => self.and_byte_reg(Reg8::D),
            0xA3 => self.and_byte_reg(Reg8::E),
            0xA4 => self.and_byte_reg(Reg8::H),
            0xA5 => self.and_byte_reg(Reg8::L),
            0xE6 => self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::A, Some(PostOp::And(Reg8::A)))),
            0xA6 => self.enqueue(MicroOp::MemRead(AddressMode::Indirect(Reg16::HL), Reg8::TempHi, Some(PostOp::And(Reg8::TempHi)))),
            0xAF => self.xor_byte_reg(Reg8::A),
            0xA8 => self.xor_byte_reg(Reg8::B),
            0xA9 => self.xor_byte_reg(Reg8::C),
            0xAA => self.xor_byte_reg(Reg8::D),
            0xAB => self.xor_byte_reg(Reg8::E),
            0xAC => self.xor_byte_reg(Reg8::H),
            0xAD => self.xor_byte_reg(Reg8::L),
            0xEE => self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::A, Some(PostOp::Xor(Reg8::A)))),
            0xAE => self.enqueue(MicroOp::MemRead(AddressMode::Indirect(Reg16::HL), Reg8::TempHi, Some(PostOp::Xor(Reg8::TempHi)))),
            0xB7 => self.or_byte_reg(Reg8::A),
            0xB0 => self.or_byte_reg(Reg8::B),
            0xB1 => self.or_byte_reg(Reg8::C),
            0xB2 => self.or_byte_reg(Reg8::D),
            0xB3 => self.or_byte_reg(Reg8::E),
            0xB4 => self.or_byte_reg(Reg8::H),
            0xB5 => self.or_byte_reg(Reg8::L),
            0xF6 => self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::A, Some(PostOp::Or(Reg8::A)))),
            0xB6 => self.enqueue(MicroOp::MemRead(AddressMode::Indirect(Reg16::HL), Reg8::TempHi, Some(PostOp::Or(Reg8::TempHi)))),
            0xBF => self.cmp_byte_reg(Reg8::A),
            0xB8 => self.cmp_byte_reg(Reg8::B),
            0xB9 => self.cmp_byte_reg(Reg8::C),
            0xBA => self.cmp_byte_reg(Reg8::D),
            0xBB => self.cmp_byte_reg(Reg8::E),
            0xBC => self.cmp_byte_reg(Reg8::H),
            0xBD => self.cmp_byte_reg(Reg8::L),
            0xFE => self.enqueue(MicroOp::MemRead(AddressMode::Immediate, Reg8::A, Some(PostOp::Cmp(Reg8::A)))),
            0xBE => self.enqueue(MicroOp::MemRead(AddressMode::Indirect(Reg16::HL), Reg8::TempHi, Some(PostOp::Cmp(Reg8::TempHi)))),
            0x3C => self.inc_byte_reg(Reg8::A),
            0x04 => self.inc_byte_reg(Reg8::B),
            0x0C => self.inc_byte_reg(Reg8::C),
            0x14 => self.inc_byte_reg(Reg8::D),
            0x1C => self.inc_byte_reg(Reg8::E),
            0x24 => self.inc_byte_reg(Reg8::H),
            0x2C => self.inc_byte_reg(Reg8::L),
////                [0x34] => Ok(Some(Op::IncIndHL)),
            0x3D => self.dec_byte_reg(Reg8::A),
            0x05 => self.dec_byte_reg(Reg8::B),
            0x0D => self.dec_byte_reg(Reg8::C),
            0x15 => self.dec_byte_reg(Reg8::D),
            0x1D => self.dec_byte_reg(Reg8::E),
            0x25 => self.dec_byte_reg(Reg8::H),
            0x2D => self.dec_byte_reg(Reg8::L),
////                [0x35] => Ok(Some(Op::DecIndHL)),
////                [0x27] => Ok(Some(Op::Daa)),
////                [0x2F] => Ok(Some(Op::Cpl)),
////                [0x09] => Ok(Some(Op::Add16HL(Reg16::BC))),
////                [0x19] => Ok(Some(Op::Add16HL(Reg16::DE))),
////                [0x29] => Ok(Some(Op::Add16HL(Reg16::HL))),
////                [0x39] => Ok(Some(Op::Add16HL(Reg16::SP))),
////                [0x03] => Ok(Some(Op::Inc16(Reg16::BC))),
////                [0x13] => Ok(Some(Op::Inc16(Reg16::DE))),
////                [0x23] => Ok(Some(Op::Inc16(Reg16::HL))),
////                [0x33] => Ok(Some(Op::Inc16(Reg16::SP))),
////                [0x0B] => Ok(Some(Op::Dec16(Reg16::BC))),
////                [0x1B] => Ok(Some(Op::Dec16(Reg16::DE))),
////                [0x2B] => Ok(Some(Op::Dec16(Reg16::HL))),
////                [0x3B] => Ok(Some(Op::Dec16(Reg16::SP))),
////                [0xE8, offset] => Ok(Some(Op::AddSignedSP(*offset))),
////                [0xF8, offset] => Ok(Some(Op::LoadSigned(*offset))),
////                [0x3F] => Ok(Some(Op::Ccf)),
////                [0x37] => Ok(Some(Op::Scf)),
            0x00 => {}
            0x76 => self.halted = true,
            0x10 => self.halted = true,
            0xF3 => self.interrupt_enable_master = false,
            0xFB => {
                self.interrupt_enable_master = true;
                // The effect of EI is delayed by one instruction.
                // This means that EI followed immediately by DI
                // does not allow interrupts between the EI and the DI.
                // http://gbdev.gg8.se/wiki/articles/Interrupts
                self.enqueue(MicroOp::FetchAndDecode);
            }
////                [0xC3, h, l] => Ok(Some(Op::Jump(util::make_word(*h, *l)))),
////                [0xE9] => Ok(Some(Op::JumpInd)),
////                [0x18, offset] => Ok(Some(Op::JumpRel(*offset))),
            0xCD => {
                // Call
                //
            }
////                [0xC9] => Ok(Some(Op::Ret)),
            0xD9 => {
                // RET
                // EI
            }
            0xC7 => {
                // Call
                //0x00
            }
            0xCF => {
                //0x08
            }
            0xD7 => {
                //0x10
            }
            0xDF => {
                //0x18
            }
            0xE7 => {
                //0x20
            }
            0xEF => {
                //0x28
            }
            0xF7 => {
                //0x30
            }
            0xFF => {
                //0x38
            }
            _ => unimplemented!()
        }
        Ok(())
    }

    #[inline]
    fn decode_prefix(&mut self, opcode: u8) -> Result<(), Error> {
        match opcode {
            _ => unimplemented!()
        }
    }

    #[inline]
    fn enqueue(&mut self, micro_op: MicroOp) {
        self.op_queue.push_back(micro_op);
    }

    #[inline]
    fn dequeue(&mut self) -> MicroOp {
        match self.op_queue.pop_front() {
            None => {
                if self.interrupt_enable_master {
                    MicroOp::IrqEnablePoll
                } else {
                    MicroOp::FetchAndDecode
                }
            }
            Some(op) => op
        }
    }

    #[inline]
    fn read_and_inc_pc(&mut self) -> u16 {
        let pc = self.reg_read_word(Reg16::PC);
        self.reg_write_word(Reg16::PC, pc + 1);
        pc
    }

    #[inline]
    fn mem_read(&self, addr: u16) -> Result<u8, memory::Error> {
        self.memory.borrow().read(addr)
    }

    #[inline]
    fn mem_write(&mut self, addr: u16, byte: u8) -> Result<(), memory::Error> {
        self.memory.borrow_mut().write(addr, byte)
    }

    #[inline]
    fn load_reg(&mut self, dst: Reg8, src: Reg8) {
        let byte = self.reg_read_byte(src);
        self.reg_write_byte(dst, byte);
    }

    fn get_addr(&mut self, address_mode: AddressMode) -> u16 {
        match address_mode {
            AddressMode::Immediate => self.read_and_inc_pc(),
            AddressMode::Indirect(src) => self.reg_read_word(src),
            AddressMode::IndirectIO(reg) => self.reg_read_byte(reg) as u16 + 0xFF00,
            AddressMode::IndirectInc => {
                let addr = self.reg_read_word(Reg16::HL);
                self.reg_write_word(Reg16::HL, addr.wrapping_add(1));
                addr
            }
            AddressMode::IndirectDec => {
                let addr = self.reg_read_word(Reg16::HL);
                self.reg_write_word(Reg16::HL, addr.wrapping_add(1));
                addr
            }
        }
    }

    fn add_byte_reg(&mut self, reg: Reg8) {
        let a = self.reg_read_byte(Reg8::A);
        let b = self.reg_read_byte(reg);
        self.reg_write_byte(Reg8::A, a + b)
    }

    fn add_carry_byte_reg(&mut self, reg: Reg8) {
        unimplemented!()
    }

    fn sub_byte_reg(&mut self, reg: Reg8) {
        unimplemented!()
    }

    fn sub_carry_byte_reg(&mut self, reg: Reg8) {
        unimplemented!()
    }

    fn and_byte_reg(&mut self, reg: Reg8) {
        unimplemented!()
    }

    fn xor_byte_reg(&mut self, reg: Reg8) {
        unimplemented!()
    }

    fn or_byte_reg(&mut self, reg: Reg8) {
        unimplemented!()
    }

    fn cmp_byte_reg(&mut self, reg: Reg8) {
        unimplemented!()
    }

    fn inc_byte_reg(&mut self, reg: Reg8) {
        self.set_flag(Flag::H, false);
        let byte = self.reg_read_byte(reg);

        if byte == 0xFF {
            self.reg_write_byte(reg, 0x00);
            self.set_flag(Flag::Z, true);
            self.set_flag(Flag::C, true);
        } else {
            self.reg_write_byte(reg, byte + 1);
            self.set_flag(Flag::Z, false);
            self.set_flag(Flag::C, false);
        }
    }

    fn dec_byte_reg(&mut self, reg: Reg8) {
        self.set_flag(Flag::H, true);
        let byte = self.reg_read_byte(reg);

        if byte == 0x00 {
            self.reg_write_byte(reg, 0xFF);
            self.set_flag(Flag::Z, false);
            self.set_flag(Flag::C, true);
        } else {
            self.reg_write_byte(reg, byte - 1);
            self.set_flag(Flag::Z, true);
            self.set_flag(Flag::C, false);
        }
    }

    fn reg_read_byte(&self, reg: Reg8) -> u8 {
        match reg {
            Reg8::A => self.registers[0].get_hi(),
            Reg8::B => self.registers[1].get_hi(),
            Reg8::C => self.registers[1].get_low(),
            Reg8::D => self.registers[2].get_hi(),
            Reg8::E => self.registers[2].get_low(),
            Reg8::H => self.registers[3].get_hi(),
            Reg8::L => self.registers[3].get_low(),
            Reg8::SPHi => self.sp.get_hi(),
            Reg8::SPLow => self.sp.get_low(),
            Reg8::PCHi => self.pc.get_hi(),
            Reg8::PCLo => self.pc.get_low(),
            Reg8::TempHi => self.temp_reg.get_hi(),
            Reg8::TempLow => self.temp_reg.get_low(),
        }
    }

    fn reg_read_word(&self, reg: Reg16) -> u16 {
        match reg {
            Reg16::AF => self.registers[0],
            Reg16::BC => self.registers[1],
            Reg16::DE => self.registers[2],
            Reg16::HL => self.registers[3],
            Reg16::SP => self.sp,
            Reg16::PC => self.pc,
            Reg16::Temp => self.temp_reg,
        }
    }

    fn reg_write_byte(&mut self, reg: Reg8, byte: u8) {
        match reg {
            Reg8::A => self.registers[0].set_hi(byte),
            Reg8::B => self.registers[1].set_hi(byte),
            Reg8::C => self.registers[1].set_low(byte),
            Reg8::D => self.registers[2].set_hi(byte),
            Reg8::E => self.registers[2].set_low(byte),
            Reg8::H => self.registers[3].set_hi(byte),
            Reg8::L => self.registers[3].set_low(byte),
            Reg8::SPHi => self.sp.set_hi(byte),
            Reg8::SPLow => self.sp.set_low(byte),
            Reg8::PCHi => self.pc.set_hi(byte),
            Reg8::PCLo => self.pc.set_low(byte),
            Reg8::TempHi => self.temp_reg.set_hi(byte),
            Reg8::TempLow => self.temp_reg.set_low(byte),
        }
    }

    fn reg_write_word(&mut self, reg: Reg16, word: u16) {
        match reg {
            Reg16::AF => self.registers[0] = word,
            Reg16::BC => self.registers[1] = word,
            Reg16::DE => self.registers[2] = word,
            Reg16::HL => self.registers[3] = word,
            Reg16::SP => self.sp = word,
            Reg16::PC => self.pc = word,
            Reg16::Temp => self.temp_reg = word,
        }
    }


    fn get_flag(&self, flag: Flag) -> bool {
        match flag {
            Flag::C => { false }
            Flag::H => { false }
            Flag::N => { false }
            Flag::Z => { false }
        }
    }

    fn set_flag(&mut self, flag: Flag, value: bool) {
        match flag {
            Flag::C => {
                let mut byte = self.registers[0].get_hi();
                byte.set_bit(0, value as usize);
                self.registers[0].set_hi(byte);
            }
            Flag::H => {
                let mut byte = self.registers[0].get_hi();
                byte.set_bit(0, value as usize);
                self.registers[0].set_hi(byte);
            }
            Flag::N => {
                let mut byte = self.registers[0].get_hi();
                byte.set_bit(0, value as usize);
                self.registers[0].set_hi(byte);
            }
            Flag::Z => {
                let mut byte = self.registers[0].get_hi();
                byte.set_bit(0, value as usize);
                self.registers[0].set_hi(byte);
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::ops::RangeInclusive;

    use crate::memory;
    use crate::memory::Addressable;
    use crate::memory::test_util::VecMemory;
    use crate::ops;
    use crate::ops::Op;

    use super::*;

    fn compile<Ops>(ops: Ops) -> Vec<u8> where Ops: IntoIterator<Item=Op> {
        let mut bytes = vec![];

        for op in ops {
            let v: Vec<u8> = op.into();
            bytes.extend_from_slice(v.as_slice())
        }

        bytes
    }

    #[test]
    fn should_increment_pc() {
        let memory = Rc::new(RefCell::new(VecMemory::new(compile(vec![Op::Nop, Op::Halt]))));

        let mut cpu = CPU::new(memory.clone());
        assert_eq!(0, cpu.reg_read_word(dbg!(Reg16::PC)), "PC should be 0");

        cpu.step();
        assert_eq!(1, cpu.reg_read_word(Reg16::PC), "PC should be 1");
    }

    #[test]
    fn simple_addition() {
        let memory = {
            let program = vec![
                Op::LoadImm(Reg8::A, 0x01),
                Op::LoadImm(Reg8::B, 0x03),
                Op::Load16(Reg16::HL, 0x00FF),
                Op::Add(Reg8::B),
                Op::StoreInd(Reg16::HL, Reg8::A),
                Op::Halt,
            ];

            let mut bytes = compile(program);
            bytes.resize(0x0100 + 1, 0x00);
            Rc::new(RefCell::new(VecMemory::new(bytes)))
        };

        let mut cpu = CPU::new(memory.clone());

        loop {
            println!("cpu.halted: {:?}", cpu.halted);
            println!("cpu.pc: {:?}", cpu.pc);
            println!("cpu.sp: {:?}", cpu.sp);
            println!("cpu.temp_reg_h: {:?}", cpu.temp_reg);
            println!("cpu.registers: {:?}", cpu.registers);

            match cpu.step() {
                Ok(true) => continue,
                Ok(false) => break,
                Err(err) => panic!("Error: {:?}", err)
            }
        }

        assert_eq!(Ok(0x01 + 0x03), memory.borrow().read(0x00FF));
    }
}
