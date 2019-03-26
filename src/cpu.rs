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

use crate::interrupts::InterruptController;
use crate::memory;
use crate::util;
use crate::util::{Bit, Byte};

use self::AddressMode::*;
use self::MicroOp::*;
use self::Op::*;
use self::Reg16::*;
use self::Reg8::*;

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

#[derive(Debug, Clone, Copy)]
pub(crate) enum Reg8 { A, F, B, C, D, E, H, L, SPHi, SPLow, PCHi, PCLo, TempHi, TempLow }

#[derive(Debug, Copy, Clone)]
pub(crate) enum Reg16 { AF, BC, DE, HL, SP, PC, Temp }

#[derive(Debug, Copy, Clone)]
pub(crate) enum Flag { Z, N, H, C }

#[derive(Debug, Copy, Clone)]
enum AddressMode {
    Immediate,
    Indirect(Reg16),
    IndirectIO(Reg8),
    IndirectInc(Reg16),
    IndirectDec(Reg16),
    FixedAddress(u16),
}

#[derive(Debug, Copy, Clone)]
enum MicroOp {
    Done,
    Decode,
    DecodePrefix,
    IrqService,
    Add(Reg8),
    AddCarry(Reg8),
    Sub(Reg8),
    SubCarry(Reg8),
    And(Reg8),
    Xor(Reg8),
    Or(Reg8),
    Cmp(Reg8),

//rlca           07           4 000c rotate akku left
//rla            17           4 000c rotate akku left through carry
//rrca           0F           4 000c rotate akku right
//rra            1F           4 000c rotate akku right through carry
//rlc  r         CB 0x        8 z00c rotate left
//rlc  (HL)      CB 06       16 z00c rotate left
//rl   r         CB 1x        8 z00c rotate left through carry
//rl   (HL)      CB 16       16 z00c rotate left through carry
//rrc  r         CB 0x        8 z00c rotate right
//rrc  (HL)      CB 0E       16 z00c rotate right
//rr   r         CB 1x        8 z00c rotate right through carry
//rr   (HL)      CB 1E       16 z00c rotate right through carry
//sla  r         CB 2x        8 z00c shift left arithmetic (b0=0)
//sla  (HL)      CB 26       16 z00c shift left arithmetic (b0=0)
//swap r         CB 3x        8 z000 exchange low/hi-nibble
//swap (HL)      CB 36       16 z000 exchange low/hi-nibble
//sra  r         CB 2x        8 z00c shift right arithmetic (b7=b7)
//sra  (HL)      CB 2E       16 z00c shift right arithmetic (b7=b7)
//srl  r         CB 3x        8 z00c shift right logical (b7=0)
//srl  (HL)      CB 3E       16 z00c shift right logical (b7=0)
}

#[derive(Debug, Copy, Clone)]
enum Op {
    Load(AddressMode, Reg8, MicroOp),
    Store(AddressMode, Reg8),
    Nop,
}

pub struct CPU {
    halted: bool,
    pc: u16,
    sp: u16,
    temp_reg: u16,
    registers: [u16; 4],
    memory: Rc<RefCell<dyn memory::Addressable>>,
    op_queue: VecDeque<Op>,
    interrupt_enable_master: bool,
}

impl CPU {
    pub fn new(memory: Rc<RefCell<dyn memory::Addressable>>) -> Self {
        Self {
            halted: false,
            pc: 0,
            sp: 0,
            temp_reg: 0,
            registers: [0; 4],
            memory,
            op_queue: VecDeque::with_capacity(3),
            interrupt_enable_master: false,
        }
    }

    pub fn step(&mut self) -> Result<bool, Error> {
        match self.dequeue() {
            Load(address_mode, dst, post_op) => {
                let addr = self.get_addr(address_mode);
                let byte = self.mem_read(addr)?;
                self.reg_write_byte(dst, byte);
                self.exec_op(post_op);
            }
            Store(address_mode, src) => {
                let addr = self.get_addr(address_mode);
                let byte = self.reg_read_byte(src);
                self.mem_write(addr, byte)?;
            }
            Nop => {}
        }
        Ok(!self.halted)
    }

    #[inline]
    fn enqueue(&mut self, micro_op: Op) {
        self.op_queue.push_back(micro_op);
    }

    #[inline]
    fn dequeue(&mut self) -> Op {
        self.op_queue.pop_front().unwrap_or({
            if self.interrupt_enable_master {
                self.enqueue(Load(FixedAddress(InterruptController::FLAG_IE), TempHi, Done));
                self.enqueue(Load(FixedAddress(InterruptController::FLAG_IE), TempLow, IrqService));
                self.op_queue.pop_front().expect("OP queue should contain op")
            } else {
                Load(Immediate, TempHi, Decode)
            }
        })
    }

    fn exec_op(&mut self, post_op: MicroOp) -> Result<(), Error> {
        match post_op {
            Done => {
                // No op
            }
            Decode => {
                let opcode = self.reg_read_byte(TempHi);
                self.decode(opcode)?;
            }
            DecodePrefix => {
                let opcode = self.reg_read_byte(TempHi);
                self.decode_prefix(opcode);
            }
            IrqService => {
                unimplemented!();
            }
            Add(reg) => {
                let a = self.reg_read_byte(Reg8::A);
                let b = self.reg_read_byte(reg);
                self.reg_write_byte(Reg8::A, a + b);
            }
            AddCarry(reg) => {
                unimplemented!();
            }
            Sub(reg) => {
                unimplemented!();
            }
            SubCarry(reg) => {
                unimplemented!();
            }
            And(reg) => {
                unimplemented!();
            }
            Xor(reg) => {
                unimplemented!();
            }
            Or(reg) => {
                unimplemented!();
            }
            Cmp(reg) => {
                unimplemented!();
            }
        }
        Ok(())
    }

    #[inline]
    fn decode(&mut self, opcode: u8) -> Result<(), Error> {
        match dbg!(opcode) {
            0xCB => self.enqueue(Load(Immediate, TempHi, DecodePrefix)),
            0x7F => self.load(A, A),
            0x78 => self.load(A, B),
            0x79 => self.load(A, C),
            0x7A => self.load(A, D),
            0x7B => self.load(A, E),
            0x7C => self.load(A, H),
            0x7D => self.load(A, L),
            0x47 => self.load(B, A),
            0x40 => self.load(B, B),
            0x41 => self.load(B, C),
            0x42 => self.load(B, D),
            0x43 => self.load(B, E),
            0x44 => self.load(B, H),
            0x45 => self.load(B, L),
            0x4F => self.load(C, A),
            0x48 => self.load(C, B),
            0x49 => self.load(C, C),
            0x4A => self.load(C, D),
            0x4B => self.load(C, E),
            0x4C => self.load(C, H),
            0x4D => self.load(C, L),
            0x57 => self.load(D, A),
            0x50 => self.load(D, B),
            0x51 => self.load(D, C),
            0x52 => self.load(D, D),
            0x53 => self.load(D, E),
            0x54 => self.load(D, H),
            0x55 => self.load(D, L),
            0x5F => self.load(E, A),
            0x58 => self.load(E, B),
            0x59 => self.load(E, C),
            0x5A => self.load(E, D),
            0x5B => self.load(E, E),
            0x5C => self.load(E, H),
            0x5D => self.load(E, L),
            0x67 => self.load(H, A),
            0x60 => self.load(H, B),
            0x61 => self.load(H, C),
            0x62 => self.load(H, D),
            0x63 => self.load(H, E),
            0x64 => self.load(H, H),
            0x65 => self.load(H, L),
            0x6F => self.load(L, A),
            0x68 => self.load(L, B),
            0x69 => self.load(L, C),
            0x6A => self.load(L, D),
            0x6B => self.load(L, E),
            0x6C => self.load(L, H),
            0x6D => self.load(L, L),
            0x3E => self.enqueue(Load(Immediate, A, Done)),
            0x06 => self.enqueue(Load(Immediate, B, Done)),
            0x0E => self.enqueue(Load(Immediate, C, Done)),
            0x16 => self.enqueue(Load(Immediate, D, Done)),
            0x1E => self.enqueue(Load(Immediate, E, Done)),
            0x26 => self.enqueue(Load(Immediate, H, Done)),
            0x2E => self.enqueue(Load(Immediate, L, Done)),
            0x0A => self.enqueue(Load(Indirect(BC), A, Done)),
            0x1A => self.enqueue(Load(Indirect(DE), A, Done)),
            0x7E => self.enqueue(Load(Indirect(HL), A, Done)),
            0x46 => self.enqueue(Load(Indirect(HL), B, Done)),
            0x4E => self.enqueue(Load(Indirect(HL), C, Done)),
            0x56 => self.enqueue(Load(Indirect(HL), D, Done)),
            0x5E => self.enqueue(Load(Indirect(HL), E, Done)),
            0x66 => self.enqueue(Load(Indirect(HL), H, Done)),
            0x6E => self.enqueue(Load(Indirect(HL), L, Done)),
            0xFA => {
                self.enqueue(Load(Immediate, TempHi, Done));
                self.enqueue(Load(Immediate, TempLow, Done));
                self.enqueue(Load(Indirect(Temp), A, Done));
            }
            0x02 => self.enqueue(Store(Indirect(BC), A)),
            0x12 => self.enqueue(Store(Indirect(DE), A)),
            0x77 => self.enqueue(Store(Indirect(HL), A)),
            0x70 => self.enqueue(Store(Indirect(HL), B)),
            0x71 => self.enqueue(Store(Indirect(HL), C)),
            0x72 => self.enqueue(Store(Indirect(HL), D)),
            0x73 => self.enqueue(Store(Indirect(HL), E)),
            0x74 => self.enqueue(Store(Indirect(HL), H)),
            0x75 => self.enqueue(Store(Indirect(HL), L)),
            0x36 => {
                self.enqueue(Load(Immediate, TempHi, Done));
                self.enqueue(Store(Indirect(HL), TempHi));
            }
            0xEA => {
                self.enqueue(Load(Immediate, TempHi, Done));
                self.enqueue(Load(Immediate, TempLow, Done));
                self.enqueue(Store(Indirect(Temp), A));
            }
            0xF0 => {
                self.enqueue(Load(Immediate, TempHi, Done));
                self.enqueue(Load(IndirectIO(TempHi), A, Done));
            }
            0xE0 => {
                self.enqueue(Load(Immediate, TempHi, Done));
                self.enqueue(Store(IndirectIO(TempHi), A));
            }
            0xF2 => self.enqueue(Load(IndirectIO(C), A, Done)),
            0xE2 => self.enqueue(Store(IndirectIO(C), A)),
            0x2A => self.enqueue(Load(IndirectInc(HL), A, Done)),
            0x22 => self.enqueue(Store(IndirectInc(HL), A)),
            0x3A => self.enqueue(Load(IndirectDec(HL), A, Done)),
            0x32 => self.enqueue(Store(IndirectDec(HL), A)),
            0x01 => {
                self.enqueue(Load(Immediate, B, Done));
                self.enqueue(Load(Immediate, C, Done));
            }
            0x11 => {
                self.enqueue(Load(Immediate, D, Done));
                self.enqueue(Load(Immediate, E, Done));
            }
            0x21 => {
                self.enqueue(Load(Immediate, H, Done));
                self.enqueue(Load(Immediate, L, Done));
            }
            0x31 => {
                self.enqueue(Load(Immediate, SPHi, Done));
                self.enqueue(Load(Immediate, SPLow, Done));
            }
            0xF9 => {
                // LD SP, HL
            }
            0xC5 => self.push(BC),
            0xD5 => self.push(DE),
            0xE5 => self.push(HL),
            0xF5 => self.push(AF),
            0xC1 => self.pop(BC),
            0xD1 => self.pop(DE),
            0xE1 => self.pop(HL),
            0xF1 => self.pop(AF),
            0x87 => self.exec_op(Add(A))?,
            0x80 => self.exec_op(Add(B))?,
            0x81 => self.exec_op(Add(C))?,
            0x82 => self.exec_op(Add(D))?,
            0x83 => self.exec_op(Add(E))?,
            0x84 => self.exec_op(Add(H))?,
            0x85 => self.exec_op(Add(L))?,
            0xC6 => self.enqueue(Load(Immediate, A, Add(A))),
            0x86 => self.enqueue(Load(Indirect(HL), TempHi, Add(TempHi))),
            0x8F => self.exec_op(AddCarry(A))?,
            0x88 => self.exec_op(AddCarry(B))?,
            0x89 => self.exec_op(AddCarry(C))?,
            0x8A => self.exec_op(AddCarry(D))?,
            0x8B => self.exec_op(AddCarry(E))?,
            0x8C => self.exec_op(AddCarry(H))?,
            0x8D => self.exec_op(AddCarry(L))?,
            0xCE => self.enqueue(Load(Immediate, A, AddCarry(A))),
            0x8E => self.enqueue(Load(Indirect(HL), TempHi, AddCarry(TempHi))),
            0x97 => self.exec_op(Sub(A))?,
            0x90 => self.exec_op(Sub(B))?,
            0x91 => self.exec_op(Sub(C))?,
            0x92 => self.exec_op(Sub(D))?,
            0x93 => self.exec_op(Sub(E))?,
            0x94 => self.exec_op(Sub(H))?,
            0x95 => self.exec_op(Sub(L))?,
            0xD6 => self.enqueue(Load(Immediate, A, Sub(A))),
            0x96 => self.enqueue(Load(Indirect(HL), TempHi, Sub(TempHi))),
            0x9F => self.exec_op(SubCarry(A))?,
            0x98 => self.exec_op(SubCarry(B))?,
            0x99 => self.exec_op(SubCarry(C))?,
            0x9A => self.exec_op(SubCarry(D))?,
            0x9B => self.exec_op(SubCarry(E))?,
            0x9C => self.exec_op(SubCarry(H))?,
            0x9D => self.exec_op(SubCarry(L))?,
            0xDE => self.enqueue(Load(Immediate, A, SubCarry(A))),
            0x9E => self.enqueue(Load(Indirect(HL), TempHi, SubCarry(TempHi))),
            0xA7 => self.exec_op(And(A))?,
            0xA0 => self.exec_op(And(B))?,
            0xA1 => self.exec_op(And(C))?,
            0xA2 => self.exec_op(And(D))?,
            0xA3 => self.exec_op(And(E))?,
            0xA4 => self.exec_op(And(H))?,
            0xA5 => self.exec_op(And(L))?,
            0xE6 => self.enqueue(Load(Immediate, A, And(A))),
            0xA6 => self.enqueue(Load(Indirect(HL), TempHi, And(TempHi))),
            0xAF => self.exec_op(Xor(A))?,
            0xA8 => self.exec_op(Xor(B))?,
            0xA9 => self.exec_op(Xor(C))?,
            0xAA => self.exec_op(Xor(D))?,
            0xAB => self.exec_op(Xor(E))?,
            0xAC => self.exec_op(Xor(H))?,
            0xAD => self.exec_op(Xor(L))?,
            0xEE => self.enqueue(Load(Immediate, A, Xor(A))),
            0xAE => self.enqueue(Load(Indirect(HL), TempHi, Xor(TempHi))),
            0xB7 => self.exec_op(Or(A))?,
            0xB0 => self.exec_op(Or(B))?,
            0xB1 => self.exec_op(Or(C))?,
            0xB2 => self.exec_op(Or(D))?,
            0xB3 => self.exec_op(Or(E))?,
            0xB4 => self.exec_op(Or(H))?,
            0xB5 => self.exec_op(Or(L))?,
            0xF6 => self.enqueue(Load(Immediate, A, Or(A))),
            0xB6 => self.enqueue(Load(Indirect(HL), TempHi, Or(TempHi))),
            0xBF => self.exec_op(Cmp(A))?,
            0xB8 => self.exec_op(Cmp(B))?,
            0xB9 => self.exec_op(Cmp(C))?,
            0xBA => self.exec_op(Cmp(D))?,
            0xBB => self.exec_op(Cmp(E))?,
            0xBC => self.exec_op(Cmp(H))?,
            0xBD => self.exec_op(Cmp(L))?,
            0xFE => self.enqueue(Load(Immediate, A, Cmp(A))),
            0xBE => self.enqueue(Load(Indirect(HL), TempHi, Cmp(TempHi))),
            0x3C => self.inc_byte_reg(A),
            0x04 => self.inc_byte_reg(B),
            0x0C => self.inc_byte_reg(C),
            0x14 => self.inc_byte_reg(D),
            0x1C => self.inc_byte_reg(E),
            0x24 => self.inc_byte_reg(H),
            0x2C => self.inc_byte_reg(L),
////                [0x34] => Ok(Some(Op::IncIndHL)),
            0x3D => self.dec_byte_reg(A),
            0x05 => self.dec_byte_reg(B),
            0x0D => self.dec_byte_reg(C),
            0x15 => self.dec_byte_reg(D),
            0x1D => self.dec_byte_reg(E),
            0x25 => self.dec_byte_reg(H),
            0x2D => self.dec_byte_reg(L),
////                [0x35] => Ok(Some(Op::DecIndHL)),
////                [0x27] => Ok(Some(Op::Daa)),
////                [0x2F] => Ok(Some(Op::Cpl)),
////                [0x09] => Ok(Some(Op::Add16HL(BC))),
////                [0x19] => Ok(Some(Op::Add16HL(DE))),
////                [0x29] => Ok(Some(Op::Add16HL(HL))),
////                [0x39] => Ok(Some(Op::Add16HL(SP))),
////                [0x03] => Ok(Some(Op::Inc16(BC))),
////                [0x13] => Ok(Some(Op::Inc16(DE))),
////                [0x23] => Ok(Some(Op::Inc16(HL))),
////                [0x33] => Ok(Some(Op::Inc16(SP))),
////                [0x0B] => Ok(Some(Op::Dec16(BC))),
////                [0x1B] => Ok(Some(Op::Dec16(DE))),
////                [0x2B] => Ok(Some(Op::Dec16(HL))),
////                [0x3B] => Ok(Some(Op::Dec16(SP))),
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
                self.enqueue(Load(Immediate, TempHi, Decode));
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
    fn decode_prefix(&mut self, opcode: u8) {
        match opcode {
            _ => unimplemented!()
        }
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
    fn load(&mut self, dst: Reg8, src: Reg8) {
        let byte = self.reg_read_byte(src);
        self.reg_write_byte(dst, byte);
    }

    fn push(&mut self, reg: Reg16) {
        // PUSH has an extra internal delay, which causes it to use 4 M-cycles (vs 3 cycles POP rr):
        self.enqueue(Nop);

        match reg {
            Reg16::AF => {
                self.enqueue(Store(IndirectDec(SP), A));
                self.enqueue(Store(IndirectDec(SP), F));
            }
            Reg16::BC => {
                self.enqueue(Store(IndirectDec(SP), B));
                self.enqueue(Store(IndirectDec(SP), C));
            }
            Reg16::DE => {
                self.enqueue(Store(IndirectDec(SP), D));
                self.enqueue(Store(IndirectDec(SP), E));
            }
            Reg16::HL => {
                self.enqueue(Store(IndirectDec(SP), H));
                self.enqueue(Store(IndirectDec(SP), L));
            }
            Reg16::SP => {
                // makes no sense
                self.enqueue(Store(IndirectDec(SP), A));
                self.enqueue(Store(IndirectDec(SP), F));
            }
            Reg16::PC => {
                self.enqueue(Store(IndirectDec(SP), PCHi));
                self.enqueue(Store(IndirectDec(SP), PCLo));
            }
            Reg16::Temp => {
                // wat?
            }
        }
    }

    fn pop(&mut self, reg: Reg16) {
        match reg {
            Reg16::AF => {
                self.enqueue(Load(IndirectInc(SP), F, Done));
                self.enqueue(Load(IndirectInc(SP), A, Done));
            }
            Reg16::BC => {
                self.enqueue(Load(IndirectInc(SP), C, Done));
                self.enqueue(Load(IndirectInc(SP), B, Done));
            }
            Reg16::DE => {
                self.enqueue(Load(IndirectInc(SP), E, Done));
                self.enqueue(Load(IndirectInc(SP), D, Done));
            }
            Reg16::HL => {
                self.enqueue(Load(IndirectInc(SP), L, Done));
                self.enqueue(Load(IndirectInc(SP), H, Done));
            }
            Reg16::SP => {
                // makes no sense
                self.enqueue(Load(IndirectInc(SP), F, Done));
                self.enqueue(Load(IndirectInc(SP), A, Done));
            }
            Reg16::PC => {
                self.enqueue(Load(IndirectInc(SP), PCLo, Done));
                self.enqueue(Load(IndirectInc(SP), PCHi, Done));
            }
            Reg16::Temp => {
                // wat?
            }
        }
    }


    fn get_addr(&mut self, address_mode: AddressMode) -> u16 {
        match address_mode {
            FixedAddress(addr) => addr,
            Immediate => {
                let addr = self.reg_read_word(PC);
                self.reg_write_word(PC, addr + 1);
                addr
            }
            Indirect(src) => self.reg_read_word(src),
            IndirectIO(reg) => self.reg_read_byte(reg) as u16 + 0xFF00,
            IndirectInc(reg) => {
                let addr = self.reg_read_word(reg);
                self.reg_write_word(reg, addr.wrapping_add(1));
                addr
            }
            IndirectDec(reg) => {
                let addr = self.reg_read_word(reg);
                self.reg_write_word(reg, addr.wrapping_sub(1));
                addr
            }
        }
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
            A => self.registers[0].get_hi(),
            B => self.registers[1].get_hi(),
            C => self.registers[1].get_low(),
            D => self.registers[2].get_hi(),
            E => self.registers[2].get_low(),
            H => self.registers[3].get_hi(),
            L => self.registers[3].get_low(),
            SPHi => self.sp.get_hi(),
            SPLow => self.sp.get_low(),
            PCHi => self.pc.get_hi(),
            PCLo => self.pc.get_low(),
            TempHi => self.temp_reg.get_hi(),
            TempLow => self.temp_reg.get_low(),
            F => { unimplemented!() }
        }
    }

    fn reg_read_word(&self, reg: Reg16) -> u16 {
        match reg {
            AF => self.registers[0],
            BC => self.registers[1],
            DE => self.registers[2],
            HL => self.registers[3],
            SP => self.sp,
            PC => self.pc,
            Temp => self.temp_reg,
        }
    }

    fn reg_write_byte(&mut self, reg: Reg8, byte: u8) {
        match reg {
            A => self.registers[0].set_hi(byte),
            B => self.registers[1].set_hi(byte),
            C => self.registers[1].set_low(byte),
            D => self.registers[2].set_hi(byte),
            E => self.registers[2].set_low(byte),
            H => self.registers[3].set_hi(byte),
            L => self.registers[3].set_low(byte),
            SPHi => self.sp.set_hi(byte),
            SPLow => self.sp.set_low(byte),
            PCHi => self.pc.set_hi(byte),
            PCLo => self.pc.set_low(byte),
            TempHi => self.temp_reg.set_hi(byte),
            TempLow => self.temp_reg.set_low(byte),
            F => {}
        }
    }

    fn reg_write_word(&mut self, reg: Reg16, word: u16) {
        match reg {
            AF => self.registers[0] = word,
            BC => self.registers[1] = word,
            DE => self.registers[2] = word,
            HL => self.registers[3] = word,
            SP => self.sp = word,
            PC => self.pc = word,
            Temp => self.temp_reg = word,
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
        assert_eq!(0, cpu.reg_read_word(dbg!(PC)), "PC should be 0");

        cpu.step();
        assert_eq!(1, cpu.reg_read_word(PC), "PC should be 1");
    }

    #[test]
    fn simple_addition() {
        let memory = {
            let program = vec![
                Op::LoadImm(A, 0x01),
                Op::LoadImm(B, 0x03),
                Op::Load16(HL, 0x00FF),
                Op::Add(B),
                Op::StoreInd(HL, A),
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
