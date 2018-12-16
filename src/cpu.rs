// cpu.rs

use std;
use std::fmt;
use std::cell::RefCell;
use std::rc::Rc;
use std::rc::Weak;

use crate::util;
use crate::memory;

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    UnknownOpCode,
    InvalidMemoryAccess,
}

#[derive(Debug)]
enum Reg8 { A, B, C, D, E, H, L }

#[derive(Debug)]
enum Reg16 { BC, DE, HL }

#[derive(Debug)]
enum Flag { Z, N, H, C }

#[derive(Debug)]
enum Register { Register8(Reg8), Register16(Reg16), Flag(Flag) }

#[derive(Debug)]
enum Op {
    // 8-bit load/store
    Load(Reg8, Reg8),
    LoadImm(Reg8, u8),
    LoadInd(Reg8, Reg16),
    LoadIndImm(Reg8, u16),
    StoreInd(Reg16, Reg8),
    StoreImmediate(Reg16, u8),
    StoreIndImmediate(u16, Reg8),
    LoadIO(u8),
    StoreIO(u8),
    LoadIOC,
    StoreIOC,
    LoadInc,
    StoreInc,
    LoadDec,
    StoreDec,
    // 16-bit load
    Load16(Reg16, u16),
    LoadSP,
    Push(Reg16),
    Pop(Reg16),
    // 8-bit arithmetic
    Add(Reg8),
    AddImmediate(u8),
    AddIndHL,
    AddCarry(Reg8),
    AddCarryImmediate(u8),
    AddCarryIndHL,
    Sub(Reg8),
    SubImmediate(u8),
    SubIndHL,
    SubCarry(Reg8),
    SubCarryImmediate(u8),
    SubCarryIndHL,
    And(Reg8),
    AndImmediate(u8),
    AndIndHL,
    Xor(Reg8),
    XorImmediate(u8),
    XorIndHL,
    Or(Reg8),
    OrImmediate(u8),
    OrIndHL,
    Cmp(Reg8),
    CmpImmediate(u8),
    CmpIndHL,
    Inc(Reg8),
    IncIndHL,
    Dec(Reg8),
    DecIndHL,
    Daa,
    Cpl,
    // 16-bit arithmetic
    Add16HL(Reg16),
    Inc16(Reg16),
    Dec16(Reg16),
    AddSignedSP(u8),
    LoadSigned(u8),
    // Rotate/shift
    // Single bit
    Bit(u8, u16),
    BitHL(u8),
    Set(u8, u16),
    SetHL(u8),
    Res(u8, u16),
    ResHL(u8),
    // Control
    Ccf,
    Scf,
    Nop,
    Halt,
    Stop,
    Di,
    Ei,
    // Jump
    Jump(u16),
    JumpInd,
    JumpCond(Flag, u16),
    JumpRel(u8),
    JumpRelCond(Flag, u8),
    Call(u16),
    CallCond(Flag, u16),
    Ret,
    RetCond(Flag),
    RetEnable,
    Reset(u8),
}

impl Into<Vec<u8>> for Op {
    fn into(self) -> Vec<u8> {
        match self {
//            Op::Load(Reg8::A, Reg8::A) => vec![0x00],
//            Op::Load(Reg8::A, Reg8::B) => vec![0x00],
//            Op::Load(Reg8::A, Reg8::C) => vec![0x00],
//            Op::Load(Reg8::A, Reg8::D) => vec![0x00],
//            Op::Load(Reg8::A, Reg8::E) => vec![0x00],
//            Op::Load(Reg8::A, Reg8::H) => vec![0x00],
//            Op::Load(Reg8::A, Reg8::L) => vec![0x00],
//            Op::Load(Reg8::B, Reg8::A) => vec![0x00],
//            Op::Load(Reg8::B, Reg8::B) => vec![0x00],
//            Op::Load(Reg8::B, Reg8::C) => vec![0x00],
//            Op::Load(Reg8::B, Reg8::D) => vec![0x00],
//            Op::Load(Reg8::B, Reg8::E) => vec![0x00],
//            Op::Load(Reg8::B, Reg8::H) => vec![0x00],
//            Op::Load(Reg8::B, Reg8::L) => vec![0x00],
//            Op::Load(Reg8::C, Reg8::A) => vec![0x00],
//            Op::Load(Reg8::C, Reg8::B) => vec![0x00],
//            Op::Load(Reg8::C, Reg8::C) => vec![0x00],
//            Op::Load(Reg8::C, Reg8::D) => vec![0x00],
//            Op::Load(Reg8::C, Reg8::E) => vec![0x00],
//            Op::Load(Reg8::C, Reg8::H) => vec![0x00],
//            Op::Load(Reg8::C, Reg8::L) => vec![0x00],
//            Op::Load(Reg8::D, Reg8::A) => vec![0x00],
//            Op::Load(Reg8::D, Reg8::B) => vec![0x00],
//            Op::Load(Reg8::D, Reg8::C) => vec![0x00],
//            Op::Load(Reg8::D, Reg8::D) => vec![0x00],
//            Op::Load(Reg8::D, Reg8::E) => vec![0x00],
//            Op::Load(Reg8::D, Reg8::H) => vec![0x00],
//            Op::Load(Reg8::D, Reg8::L) => vec![0x00],
//            Op::Load(Reg8::E, Reg8::A) => vec![0x00],
//            Op::Load(Reg8::E, Reg8::B) => vec![0x00],
//            Op::Load(Reg8::E, Reg8::C) => vec![0x00],
//            Op::Load(Reg8::E, Reg8::D) => vec![0x00],
//            Op::Load(Reg8::E, Reg8::E) => vec![0x00],
//            Op::Load(Reg8::E, Reg8::H) => vec![0x00],
//            Op::Load(Reg8::E, Reg8::L) => vec![0x00],
//            Op::Load(Reg8::H, Reg8::A) => vec![0x00],
//            Op::Load(Reg8::H, Reg8::B) => vec![0x00],
//            Op::Load(Reg8::H, Reg8::C) => vec![0x00],
//            Op::Load(Reg8::H, Reg8::D) => vec![0x00],
//            Op::Load(Reg8::H, Reg8::E) => vec![0x00],
//            Op::Load(Reg8::H, Reg8::H) => vec![0x00],
//            Op::Load(Reg8::H, Reg8::L) => vec![0x00],
//            Op::Load(Reg8::L, Reg8::A) => vec![0x00],
//            Op::Load(Reg8::L, Reg8::B) => vec![0x00],
//            Op::Load(Reg8::L, Reg8::C) => vec![0x00],
//            Op::Load(Reg8::L, Reg8::D) => vec![0x00],
//            Op::Load(Reg8::L, Reg8::E) => vec![0x00],
//            Op::Load(Reg8::L, Reg8::H) => vec![0x00],
//            Op::Load(Reg8::L, Reg8::L) => vec![0x00],
            Op::LoadImm(Reg8::A, byte) => vec![0x3E, byte],
            Op::LoadImm(Reg8::B, byte) => vec![0x06, byte],
//            Op::LoadImm(Reg8::C, byte) => vec![0x00, byte],
//            Op::LoadImm(Reg8::D, byte) => vec![0x00, byte],
//            Op::LoadImm(Reg8::E, byte) => vec![0x00, byte],
//            Op::LoadImm(Reg8::H, byte) => vec![0x00, byte],
//            Op::LoadImm(Reg8::L, byte) => vec![0x00, byte],
//            Op::LoadInd(Reg8::A, Reg16::BC) => vec![0x00],
//            Op::LoadInd(Reg8::A, Reg16::DE) => vec![0x00],
//            Op::LoadInd(Reg8::A, Reg16::HL) => vec![0x00],
//            Op::LoadInd(Reg8::B, Reg16::BC) => vec![0x00],
//            Op::LoadInd(Reg8::B, Reg16::DE) => vec![0x00],
//            Op::LoadInd(Reg8::B, Reg16::HL) => vec![0x00],
//            Op::LoadInd(Reg8::C, Reg16::BC) => vec![0x00],
//            Op::LoadInd(Reg8::C, Reg16::DE) => vec![0x00],
//            Op::LoadInd(Reg8::C, Reg16::HL) => vec![0x00],
//            Op::LoadInd(Reg8::D, Reg16::BC) => vec![0x00],
//            Op::LoadInd(Reg8::D, Reg16::DE) => vec![0x00],
//            Op::LoadInd(Reg8::D, Reg16::HL) => vec![0x00],
//            Op::LoadInd(Reg8::E, Reg16::BC) => vec![0x00],
//            Op::LoadInd(Reg8::E, Reg16::DE) => vec![0x00],
//            Op::LoadInd(Reg8::E, Reg16::HL) => vec![0x00],
//            Op::LoadInd(Reg8::H, Reg16::BC) => vec![0x00],
//            Op::LoadInd(Reg8::H, Reg16::DE) => vec![0x00],
//            Op::LoadInd(Reg8::H, Reg16::HL) => vec![0x00],
//            Op::LoadInd(Reg8::L, Reg16::BC) => vec![0x00],
//            Op::LoadInd(Reg8::L, Reg16::DE) => vec![0x00],
//            Op::LoadInd(Reg8::L, Reg16::HL) => vec![0x00],
//            Op::LoadIndImm(Reg8, word) => vec![0x00],
//            Op::StoreInd(Reg16::BC, Reg8::A) => vec![0x00],
//            Op::StoreInd(Reg16::BC, Reg8::B) => vec![0x00],
//            Op::StoreInd(Reg16::BC, Reg8::C) => vec![0x00],
//            Op::StoreInd(Reg16::BC, Reg8::D) => vec![0x00],
//            Op::StoreInd(Reg16::BC, Reg8::E) => vec![0x00],
//            Op::StoreInd(Reg16::BC, Reg8::H) => vec![0x00],
//            Op::StoreInd(Reg16::BC, Reg8::L) => vec![0x00],
//            Op::StoreInd(Reg16::DE, Reg8::A) => vec![0x00],
//            Op::StoreInd(Reg16::DE, Reg8::B) => vec![0x00],
//            Op::StoreInd(Reg16::DE, Reg8::C) => vec![0x00],
//            Op::StoreInd(Reg16::DE, Reg8::D) => vec![0x00],
//            Op::StoreInd(Reg16::DE, Reg8::E) => vec![0x00],
//            Op::StoreInd(Reg16::DE, Reg8::H) => vec![0x00],
//            Op::StoreInd(Reg16::DE, Reg8::L) => vec![0x00],
            Op::StoreInd(Reg16::HL, Reg8::A) => vec![0x77],
//            Op::StoreInd(Reg16::HL, Reg8::B) => vec![0x00],
//            Op::StoreInd(Reg16::HL, Reg8::C) => vec![0x00],
//            Op::StoreInd(Reg16::HL, Reg8::D) => vec![0x00],
//            Op::StoreInd(Reg16::HL, Reg8::E) => vec![0x00],
//            Op::StoreInd(Reg16::HL, Reg8::H) => vec![0x00],
//            Op::StoreInd(Reg16::HL, Reg8::L) => vec![0x00],
//            Op::StoreImmediate(Register16, u8) => vec![0x00],
//            Op::StoreIndImmediate(u16, Reg8) => vec![0x00],
//            Op::LoadIO(u8) => vec![0x00],
//            Op::StoreIO(u8) => vec![0x00],
//            Op::LoadIOC => vec![0x00],
//            Op::StoreIOC => vec![0x00],
//            Op::LoadInc => vec![0x00],
//            Op::StoreInc => vec![0x00],
//            Op::LoadDec => vec![0x00],
//            Op::StoreDec => vec![0x00],
//            Op::Load16(Reg16::BC, u16) => vec![0x00],
//            Op::Load16(Reg16::DE, u16) => vec![0x00],
            Op::Load16(Reg16::HL, word) => {
                let h = (word >> 8) as u8;
                let l = (word & 0x00FF) as u8;
                vec![0x21, h, l]
            }
//            Op::LoadSP => vec![0x00],
//            Op::Push(Register16) => vec![0x00],
//            Op::Pop(Register16) => vec![0x00],
            Op::Add(Reg8::B) => vec![0x80],
//            Op::AddImmediate(u8) => vec![0x00],
//            Op::AddIndHL => vec![0x00],
//            Op::AddCarry(Reg8) => vec![0x00],
//            Op::AddCarryImmediate(u8) => vec![0x00],
//            Op::AddCarryIndHL => vec![0x00],
//            Op::Sub(Reg8) => vec![0x00],
//            Op::SubImmediate(u8) => vec![0x00],
//            Op::SubIndHL => vec![0x00],
//            Op::SubCarry(Reg8) => vec![0x00],
//            Op::SubCarryImmediate(u8) => vec![0x00],
//            Op::SubCarryIndHL => vec![0x00],
//            Op::And(Reg8) => vec![0x00],
//            Op::AndImmediate(u8) => vec![0x00],
//            Op::AndIndHL => vec![0x00],
//            Op::Xor(Reg8) => vec![0x00],
//            Op::XorImmediate(u8) => vec![0x00],
//            Op::XorIndHL => vec![0x00],
//            Op::Or(Reg8) => vec![0x00],
//            Op::OrImmediate(u8) => vec![0x00],
//            Op::OrIndHL => vec![0x00],
//            Op::Cmp(Reg8) => vec![0x00],
//            Op::CmpImmediate(u8) => vec![0x00],
//            Op::CmpIndHL => vec![0x00],
//            Op::Inc(Reg8) => vec![0x00],
//            Op::IncIndHL => vec![0x00],
//            Op::Dec(Reg8) => vec![0x00],
//            Op::DecIndHL => vec![0x00],
//            Op::Daa => vec![0x00],
//            Op::Cpl => vec![0x00],
//            Op::Add16HL(Register16) => vec![0x00],
//            Op::Inc16(Register16) => vec![0x00],
//            Op::Dec16(Register16) => vec![0x00],
//            Op::AddSignedSP(u8) => vec![0x00],
//            Op::LoadSigned(u8) => vec![0x00],
//            Op::Bit(u8, u16) => vec![0x00],
//            Op::BitHL(u8) => vec![0x00],
//            Op::Set(u8, u16) => vec![0x00],
//            Op::SetHL(u8) => vec![0x00],
//            Op::Res(u8, u16) => vec![0x00],
//            Op::ResHL(u8) => vec![0x00],
//            Op::Ccf => vec![0x00],
//            Op::Scf => vec![0x00],
            Op::Nop => vec![0x00],
            Op::Halt => vec![0x76],
//            Op::Stop => vec![0x00],
//            Op::Di => vec![0x00],
//            Op::Ei => vec![0x00],
//            Op::Jump(u16) => vec![0x00],
//            Op::JumpInd => vec![0x00],
//            Op::JumpCond(Flag, u16) => vec![0x00],
//            Op::JumpRel(u8) => vec![0x00],
//            Op::JumpRelCond(Flag, u8) => vec![0x00],
//            Op::Call(u16) => vec![0x00],
//            Op::CallCond(Flag, u16) => vec![0x00],
//            Op::Ret => vec![0x00],
//            Op::RetCond(Flag) => vec![0x00],
//            Op::RetEnable => vec![0x00],
//            Op::Reset(u8) => vec![0x00],
            other => unimplemented!("No binary representation for {}", other)
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Op::Load(dest, source) => write!(f, "LD {:?}, {:?}", dest, source),
            Op::LoadImm(dest, byte) => write!(f, "LD {:?}, {:#04x}", dest, byte),
//            Op::LoadInd(Reg8, Register16) => write!(f, ""),
//            Op::LoadIndImm(Reg8, u16) => write!(f, ""),
            Op::StoreInd(Reg16::HL, Reg8::A) => write!(f, "LD (HL), A"),
//            Op::StoreImmediate(Register16, u8) => write!(f, ""),
//            Op::StoreIndImmediate(u16, Reg8) => write!(f, ""),
//            Op::LoadIO(u8) => write!(f, ""),
//            Op::StoreIO(u8) => write!(f, ""),
//            Op::LoadIOC => write!(f, ""),
//            Op::StoreIOC => write!(f, ""),
//            Op::LoadInc => write!(f, ""),
//            Op::StoreInc => write!(f, ""),
//            Op::LoadDec => write!(f, ""),
//            Op::StoreDec => write!(f, ""),
            Op::Load16(dest, src) => write!(f, "LD {:?}, {:#06x}", dest, src),
//            Op::LoadSP => write!(f, ""),
//            Op::Push(Register16) => write!(f, ""),
//            Op::Pop(Register16) => write!(f, ""),
            Op::Add(reg) => write!(f, "ADD A, {:?}", reg),
//            Op::AddImmediate(u8) => write!(f, ""),
//            Op::AddIndHL => write!(f, ""),
//            Op::AddCarry(Reg8) => write!(f, ""),
//            Op::AddCarryImmediate(u8) => write!(f, ""),
//            Op::AddCarryIndHL => write!(f, ""),
//            Op::Sub(Reg8) => write!(f, ""),
//            Op::SubImmediate(u8) => write!(f, ""),
//            Op::SubIndHL => write!(f, ""),
//            Op::SubCarry(Reg8) => write!(f, ""),
//            Op::SubCarryImmediate(u8) => write!(f, ""),
//            Op::SubCarryIndHL => write!(f, ""),
//            Op::And(Reg8) => write!(f, ""),
//            Op::AndImmediate(u8) => write!(f, ""),
//            Op::AndIndHL => write!(f, ""),
//            Op::Xor(Reg8) => write!(f, ""),
//            Op::XorImmediate(u8) => write!(f, ""),
//            Op::XorIndHL => write!(f, ""),
//            Op::Or(Reg8) => write!(f, ""),
//            Op::OrImmediate(u8) => write!(f, ""),
//            Op::OrIndHL => write!(f, ""),
//            Op::Cmp(Reg8) => write!(f, ""),
//            Op::CmpImmediate(u8) => write!(f, ""),
//            Op::CmpIndHL => write!(f, ""),
//            Op::Inc(Reg8) => write!(f, ""),
//            Op::IncIndHL => write!(f, ""),
//            Op::Dec(Reg8) => write!(f, ""),
//            Op::DecIndHL => write!(f, ""),
            Op::Daa => write!(f, "DAA"),
            Op::Cpl => write!(f, "CPL"),
//            Op::Add16HL(Register16) => write!(f, ""),
//            Op::Inc16(Register16) => write!(f, ""),
//            Op::Dec16(Register16) => write!(f, ""),
//            Op::AddSignedSP(u8) => write!(f, ""),
//            Op::LoadSigned(u8) => write!(f, ""),
//            Op::Bit(u8, u16) => write!(f, ""),
//            Op::BitHL(u8) => write!(f, ""),
//            Op::Set(u8, u16) => write!(f, ""),
//            Op::SetHL(u8) => write!(f, ""),
//            Op::Res(u8, u16) => write!(f, ""),
//            Op::ResHL(u8) => write!(f, ""),
            Op::Ccf => write!(f, "CCF"),
            Op::Scf => write!(f, "SCF"),
            Op::Nop => write!(f, "NOP"),
            Op::Halt => write!(f, "HLT"),
            Op::Stop => write!(f, "STOP"),
            Op::Di => write!(f, "DI"),
            Op::Ei => write!(f, "EI"),
//            Op::Jump(u16) => write!(f, ""),
//            Op::JumpInd => write!(f, ""),
//            Op::JumpCond(Flag, u16) => write!(f, ""),
//            Op::JumpRel(u8) => write!(f, ""),
//            Op::JumpRelCond(Flag, u8) => write!(f, ""),
//            Op::Call(u16) => write!(f, ""),
//            Op::CallCond(Flag, u16) => write!(f, ""),
            Op::Ret => write!(f, "RET"),
//            Op::RetCond(Flag) => write!(f, ""),
//            Op::RetEnable => write!(f, ""),
//            Op::Reset(u8) => write!(f, ""),
            other => write!(f, "Op::{:?}", other)
        }
    }
}

fn compile<Ops: IntoIterator<Item=Op>>(ops: Ops) -> Vec<u8> {
    let mut bytes = vec![];

    for op in ops {
        let v: Vec<u8> = op.into();
        bytes.extend_from_slice(v.as_slice())
    }

    bytes
}

enum State {
    Fetch,
    Decode,
    Exec,
}

pub struct Cpu {
    pc: u16,
    sp: u16,
    ticks: u32,
    registers: [u16; 4],
    interrupt_enable_master: bool,
    memory: Rc<RefCell<memory::Addressable>>,
}

impl Cpu {
    pub fn new(memory: Rc<RefCell<dyn memory::Addressable>>) -> Self {
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

            match Cpu::decode(&buf) {
                Ok(Some(op)) => break op,
                Ok(None) => continue,
                Err(err) => return Err(err),
            }
        };

        #[cfg(test)]
            {
                println!("Exec op: {}", op)
            }

        self.exec(op)
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
        // Process interrupts
        if self.interrupt_enable_master {
            let memory = self.memory.borrow();
            let interrupt_enable_flags = memory.get(0xFFFF).unwrap_or_else(|err| panic!(err));
            let interrupt_flags = memory.get(0xFF0F).unwrap_or_else(|err| panic!(err));

            if util::bit_enabled(interrupt_enable_flags, 0) && util::bit_enabled(interrupt_flags, 0) {
                // VBlank
                self.sp = self.pc;
                self.pc = 0x0040;
            }
        }

        Ok(true)
    }


    fn decode(bytes: &[u8]) -> Result<Option<Op>, Error> {
        match bytes {
            [0x00] => Ok(Some(Op::Nop)),
            [0x76] => Ok(Some(Op::Halt)),
            [0x10, 0x00] => Ok(Some(Op::Stop)),
            [0x80] => Ok(Some(Op::Add(Reg8::B))),
            [0x3E, val] => Ok(Some(Op::LoadImm(Reg8::A, val.clone()))),
            [0x06, val] => Ok(Some(Op::LoadImm(Reg8::B, val.clone()))),
            [0x21, h, l] => {
                let h: u16 = h.clone() as u16;
                let l: u16 = l.clone() as u16;
                let word: u16 = (h << 8) + l;
                Ok(Some(Op::Load16(Reg16::HL, word)))
            }
                [0x77] =>
            Ok(Some(Op::StoreInd(Reg16::HL, Reg8::A))),
            _ => {
                if bytes.len() > 3 {
                    panic!("Unimplemented {:02x?}", &bytes)
                } else {
                    Ok(None)
                }
            }
        }
    }

    fn exec(&mut self, op: Op) -> Result<bool, Error> {
        match op {
            Op::Load(dest, source) => {
                self.set_byte_register(dest, self.get_byte_register(source));
                Ok(true)
            }
            Op::LoadImm(dest, byte) => {
                self.set_byte_register(dest, byte);
                Ok(true)
            }
            Op::LoadInd(dest, src) => {
                let src_addr = self.get_word_register(src);

                let byte = match self.memory.borrow().get(src_addr) {
                    Ok(byte) => byte,
                    Err(_) => return Err(Error::InvalidMemoryAccess),
                };

                self.set_byte_register(dest, byte);
                Ok(true)
            }
//            Op::LoadIndImm(Reg8, u16) => Ok(false),
            Op::StoreInd(dest, src) => {
                let dest_addr = self.get_word_register(dest);
                let byte = self.get_byte_register(src);
                match self.memory.borrow_mut().set(dest_addr, byte) {
                    Ok(_) => Ok(true),
                    Err(_) => Err(Error::InvalidMemoryAccess)
                }
            }
//            Op::StoreImmediate(Register16, u8) => Ok(false),
//            Op::StoreIndImmediate(u16, Reg8) => Ok(false),
//            Op::LoadIO(u8) => Ok(false),
//            Op::StoreIO(u8) => Ok(false),
//            Op::LoadIOC => Ok(false),
//            Op::StoreIOC => Ok(false),
//            Op::LoadInc => Ok(false),
//            Op::StoreInc => Ok(false),
//            Op::LoadDec => Ok(false),
//            Op::StoreDec => Ok(false),
            Op::Load16(dest, word) => {
                self.set_word_register(dest, word);
                Ok(true)
            }
//            Op::LoadSP => Ok(false),
//            Op::Push(Register16) => Ok(false),
//            Op::Pop(Register16) => Ok(false),
            Op::Add(reg) => {
                self.set_byte_register(Reg8::A, self.get_byte_register(Reg8::A) + self.get_byte_register(reg));
                Ok(true)
            }
            Op::AddImmediate(byte) => {
                self.set_byte_register(Reg8::A, self.get_byte_register(Reg8::A) + byte);
                Ok(true)
            }
//            Op::AddIndHL => Ok(false),
//            Op::AddCarry(Reg8) => Ok(false),
//            Op::AddCarryImmediate(u8) => Ok(false),
//            Op::AddCarryIndHL => Ok(false),
//            Op::Sub(Reg8) => Ok(false),
//            Op::SubImmediate(u8) => Ok(false),
//            Op::SubIndHL => Ok(false),
//            Op::SubCarry(Reg8) => Ok(false),
//            Op::SubCarryImmediate(u8) => Ok(false),
//            Op::SubCarryIndHL => Ok(false),
//            Op::And(Reg8) => Ok(false),
//            Op::AndImmediate(u8) => Ok(false),
//            Op::AndIndHL => Ok(false),
//            Op::Xor(Reg8) => Ok(false),
//            Op::XorImmediate(u8) => Ok(false),
//            Op::XorIndHL => Ok(false),
//            Op::Or(Reg8) => Ok(false),
//            Op::OrImmediate(u8) => Ok(false),
//            Op::OrIndHL => Ok(false),
//            Op::Cmp(Reg8) => Ok(false),
//            Op::CmpImmediate(u8) => Ok(false),
//            Op::CmpIndHL => Ok(false),
//            Op::Inc(Reg8) => Ok(false),
//            Op::IncIndHL => Ok(false),
//            Op::Dec(Reg8) => Ok(false),
//            Op::DecIndHL => Ok(false),
//            Op::Daa => Ok(false),
//            Op::Cpl => Ok(false),
//            Op::Add16HL(Register16) => Ok(false),
//            Op::Inc16(Register16) => Ok(false),
//            Op::Dec16(Register16) => Ok(false),
//            Op::AddSignedSP(u8) => Ok(false),
//            Op::LoadSigned(u8) => Ok(false),
//            Op::Bit(u8, u16) => Ok(false),
//            Op::BitHL(u8) => Ok(false),
//            Op::Set(u8, u16) => Ok(false),
//            Op::SetHL(u8) => Ok(false),
//            Op::Res(u8, u16) => Ok(false),
//            Op::ResHL(u8) => Ok(false),
//            Op::Ccf => Ok(false),
//            Op::Scf => Ok(false),
            Op::Nop => Ok(true),
            Op::Halt => Ok(false),
            Op::Stop => Ok(false),
//            Op::Di => Ok(false),
//            Op::Ei => Ok(false),
//            Op::Jump(u16) => Ok(false),
//            Op::JumpInd => Ok(false),
//            Op::JumpCond(Flag, u16) => Ok(false),
//            Op::JumpRel(u8) => Ok(false),
//            Op::JumpRelCond(Flag, u8) => Ok(false),
//            Op::Call(u16) => Ok(false),
//            Op::CallCond(Flag, u16) => Ok(false),
//            Op::Ret => Ok(false),
//            Op::RetCond(Flag) => Ok(false),
//            Op::RetEnable => Ok(false),
//            Op::Reset(u8) => Ok(false),
            other => unimplemented!("Unimplemented op: {:?}", other)
        }
    }

    fn get_byte_register(&self, reg: Reg8) -> u8 {
        match reg {
            Reg8::A => (self.registers[0] >> 8) as u8,
            Reg8::B => (self.registers[1] >> 8) as u8,
            Reg8::C => (self.registers[1] & 0xFF) as u8,
            Reg8::D => (self.registers[2] >> 8) as u8,
            Reg8::E => (self.registers[2] & 0xFF) as u8,
            Reg8::H => (self.registers[3] >> 8) as u8,
            Reg8::L => (self.registers[3] & 0xFF) as u8,
        }
    }

    fn set_byte_register(&mut self, reg: Reg8, byte: u8) {
        match reg {
            Reg8::A => {
                let shift = (byte as u16) << 8;
                let mask = (0xFF as u16) << 8;
                let word = (!mask & self.registers[0]) | shift;
                self.registers[0] = word;
            }
            Reg8::B => {
                let shift = (byte as u16) << 8;
                let mask = (0xFF as u16) << 8;
                let word = (!mask & self.registers[1]) | shift;
                self.registers[1] = word
            }
            Reg8::C => (),
            Reg8::D => (),
            Reg8::E => (),
            Reg8::H => (),
            Reg8::L => (),
        }
    }

    fn get_word_register(&self, reg: Reg16) -> u16 {
        match reg {
            Reg16::BC => self.registers[1],
            Reg16::DE => self.registers[2],
            Reg16::HL => self.registers[3],
        }
    }

    fn set_word_register(&mut self, reg: Reg16, word: u16) {
        match reg {
            Reg16::BC => self.registers[1] = word,
            Reg16::DE => self.registers[2] = word,
            Reg16::HL => self.registers[3] = word,
        }
    }

    fn get_flag(&self, flag: Flag) -> bool {
        match flag {
            Flag::C => true,
            Flag::H => true,
            Flag::N => true,
            Flag::Z => true,
        }
    }

    fn set_flag(&mut self, flag: Flag, value: bool) {
        match flag {
            Flag::C => (),
            Flag::H => (),
            Flag::N => (),
            Flag::Z => (),
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
}
