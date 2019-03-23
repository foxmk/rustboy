use std;
use std::cell::{Cell, RefCell};
use std::fmt;
use std::io::SeekFrom::Start;
use std::ops::{Index, IndexMut, RangeInclusive};
use std::rc::Rc;
use std::rc::Weak;

use crate::memory;
use crate::util;
use std::io::{Read, Write};
use std::collections::VecDeque;
use crate::cpu::MicroOp::FetchAndDecode;
use std::fmt::Debug;

#[derive(Debug)]
pub(crate) enum Interrupt {
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

    pub fn find(enable_flags: u8, request_flags: u8) -> Option<Interrupt> {

//        match self {
//            Interrupt::VBlank => 0,
//            Interrupt::LcdStat => 1,
//            Interrupt::Timer => 2,
//            Interrupt::Serial => 3,
//            Interrupt::Joystick => 4,
//        }
        None
    }

    pub fn clear_request_flag(interrupt: Interrupt, request_flags: u8) -> Result<u8, Error> {
        let mut flags_to_save = request_flags.clone();

        match interrupt {
            Interrupt::VBlank => util::set_bit(&mut flags_to_save, 0, false),
            Interrupt::LcdStat => util::set_bit(&mut flags_to_save, 1, false),
            Interrupt::Timer => util::set_bit(&mut flags_to_save, 2, false),
            Interrupt::Serial => util::set_bit(&mut flags_to_save, 3, false),
            Interrupt::Joystick => util::set_bit(&mut flags_to_save, 4, false),
            _ => return Err(Error::InvalidMemoryAccess)
        }
        Ok(flags_to_save)
    }

    #[inline]
    fn get_leftmost_set_bit(byte: u8) -> usize {
//        ((byte as i8) & -(byte as i8)) as i64.log2 + 1
        0
    }
}

impl fmt::Display for Interrupt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.address())
    }
}


#[derive(Debug, Clone, Copy)]
pub(crate) enum Reg8 { A, B, C, D, E, H, L }

#[derive(Debug)]
pub(crate) enum Reg16 { AF, BC, DE, HL, SP, PC }

#[derive(Debug)]
pub(crate) enum Flag { Z, N, H, C }

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    UnknownOpCode,
    InvalidMemoryAccess,
}

impl From<memory::Error> for Error {
    fn from(err: memory::Error) -> Self {
        match err {
            memory::Error::Unavailable(_) => Error::InvalidMemoryAccess,
            memory::Error::OutOfBounds => Error::InvalidMemoryAccess,
        }
    }
}

#[derive(Debug)]
enum MicroOp {
    FetchAndDecode,
    IrqEnablePoll,
    Irq { enable_flags: u8 },
    FetchPrefix,
    LoadImm(Reg8),
    MemReadInd { reg: Reg8, addr_reg: Reg16 },
    MemWriteInd { addr_reg: Reg16, reg: Reg8 },
    TempLoadHighImm,
    TempLoadLowImm,
    MemReadImmLowStoreWord { reg: Reg16 },
    MemReadIndTemp { reg: Reg8 },
    MemWriteIndTempHigh { addr_reg: Reg16 },
    AddImm,
    AddCarryImm,
    SubImm,
    SubCarryImm,
    XorImm,
    OrImm,
    AndImm,
    CmpImm,
    AddInd,
    AddCarryInd,
    SubInd,
    SubCarryInd,
    XorInd,
    OrInd,
    AndInd,
    CmpInd,
    IncInd,
    DecInd,
    ReadIO,
    StoreIO,
    LoadIOC,
    StoreIOC,
    LoadInc,
    StoreInc,
    LoadDec,
    StoreDec,
//    MemWriteInd { addr: Reg16, reg: Reg8 },
//    MemRead { addr: u16, reg: Reg8 },
//    MemWrite { addr: u16, reg: Reg8 },
}

const FLAG_IE: u16 = 0xFFFF;
const FLAG_IF: u16 = 0xFF0F;

pub struct Cpu {
    halted: bool,
    pc: u16,
    sp: u16,
    temp_reg_h: u8,
    temp_reg_l: u8,
    registers: [u16; 4],
    memory: Rc<RefCell<dyn memory::Addressable>>,
    op_queue: VecDeque<MicroOp>,
    interrupt_enable_master: bool,
}

impl Cpu {
    pub fn new(memory: Rc<RefCell<dyn memory::Addressable>>) -> Self {
        let mut op_queue = VecDeque::with_capacity(3);
        op_queue.push_back(MicroOp::FetchAndDecode);
        Self {
            halted: false,
            pc: 0,
            sp: 0,
            temp_reg_h: 0,
            temp_reg_l: 0,
            registers: [0; 4],
            memory: memory.clone(),
            op_queue: op_queue,
            interrupt_enable_master: false,
        }
    }

    pub fn step(&mut self) -> Result<bool, Error> {
        match dbg!(self.dequeue()) {
            MicroOp::IrqEnablePoll => {
                let enable_flags = self.mem_read(FLAG_IE)?;
                self.enqueue(MicroOp::Irq { enable_flags });
            }
            MicroOp::Irq { enable_flags } => {
                let request_flags = self.mem_read(FLAG_IF)?;
                match Interrupt::find(enable_flags, request_flags) {
                    Some(interrupt) => {
                        self.interrupt_enable_master = false;

                        let flags_to_save = Interrupt::clear_request_flag(interrupt, request_flags)?;
                        self.mem_write(FLAG_IF, flags_to_save)?;


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
            MicroOp::LoadImm(reg) => {
                let addr = self.read_and_inc_pc();
                let byte = self.mem_read(addr)?;
                self.reg_write_byte(reg, byte);
            }
            MicroOp::FetchPrefix => {
                let addr = self.read_and_inc_pc();
                let opcode = self.mem_read(addr)?;
                self.decode_prefix(opcode)?;
            }
            MicroOp::MemReadInd { reg, addr_reg } => {
                let addr = self.reg_read_word(addr_reg);
                let byte = self.mem_read(addr)?;
                self.reg_write_byte(reg, byte);
            }
            MicroOp::MemWriteInd { addr_reg, reg } => {
                let addr = self.reg_read_word(addr_reg);
                let byte = self.reg_read_byte(reg);
                self.mem_write(addr, byte)?;
            }
            MicroOp::TempLoadHighImm => {
                let addr = self.read_and_inc_pc();
                self.temp_reg_h = self.mem_read(addr)?;
            }
            MicroOp::TempLoadLowImm => {
                let addr = self.read_and_inc_pc();
                self.temp_reg_l = self.mem_read(addr)?;
            }
            MicroOp::MemReadIndTemp { reg } => {
                let addr = util::make_word(self.temp_reg_h, self.temp_reg_l);
                let byte = self.mem_read(addr)?;
                self.reg_write_byte(reg, byte);
            }
            MicroOp::MemWriteIndTempHigh { addr_reg } => {
                let addr = self.reg_read_word(addr_reg);
                self.mem_write(addr, self.temp_reg_h)?;
            }
            MicroOp::AddImm => {
                let addr = self.read_and_inc_pc();
                self.temp_reg_h = self.mem_read(addr)?;
                self.add_byte_tmp_hi();
            }
            MicroOp::AddCarryImm => {
                let addr = self.read_and_inc_pc();
                self.temp_reg_h = self.mem_read(addr)?;
                self.add_carry_byte_tmp_hi();
            }
            MicroOp::SubImm => {
                let addr = self.read_and_inc_pc();
                self.temp_reg_h = self.mem_read(addr)?;
                self.sub_byte_tmp_hi();
            }
            MicroOp::SubCarryImm => {
                let addr = self.read_and_inc_pc();
                self.temp_reg_h = self.mem_read(addr)?;
                self.sub_carry_byte_tmp_hi();
            }
            MicroOp::XorImm => {
                let addr = self.read_and_inc_pc();
                self.temp_reg_h = self.mem_read(addr)?;
                self.xor_byte_tmp_hi();
            }
            MicroOp::OrImm => {
                let addr = self.read_and_inc_pc();
                self.temp_reg_h = self.mem_read(addr)?;
                self.or_byte_tmp_hi();
            }
            MicroOp::AndImm => {
                let addr = self.read_and_inc_pc();
                self.temp_reg_h = self.mem_read(addr)?;
                self.and_byte_tmp_hi();
            }
            MicroOp::CmpImm => {
                let addr = self.read_and_inc_pc();
                self.temp_reg_h = self.mem_read(addr)?;
                self.cmp_byte_tmp_hi();
            }
            MicroOp::AddInd => {
                let addr = self.reg_read_word(Reg16::HL);
                self.temp_reg_h = self.mem_read(addr)?;
                self.add_byte_tmp_hi();
            }
            MicroOp::AddCarryInd => {
                let addr = self.reg_read_word(Reg16::HL);
                self.temp_reg_h = self.mem_read(addr)?;
                self.add_carry_byte_tmp_hi();
            }
            MicroOp::SubInd => {
                let addr = self.reg_read_word(Reg16::HL);
                self.temp_reg_h = self.mem_read(addr)?;
                self.sub_byte_tmp_hi();
            }
            MicroOp::SubCarryInd => {
                let addr = self.reg_read_word(Reg16::HL);
                self.temp_reg_h = self.mem_read(addr)?;
                self.sub_carry_byte_tmp_hi();
            }
            MicroOp::XorInd => {
                let addr = self.reg_read_word(Reg16::HL);
                self.temp_reg_h = self.mem_read(addr)?;
                self.xor_byte_tmp_hi();
            }
            MicroOp::OrInd => {
                let addr = self.reg_read_word(Reg16::HL);
                self.temp_reg_h = self.mem_read(addr)?;
                self.or_byte_tmp_hi();
            }
            MicroOp::AndInd => {
                let addr = self.reg_read_word(Reg16::HL);
                self.temp_reg_h = self.mem_read(addr)?;
                self.and_byte_tmp_hi();
            }
            MicroOp::CmpInd => {
                let addr = self.reg_read_word(Reg16::HL);
                self.temp_reg_h = self.mem_read(addr)?;
                self.cmp_byte_tmp_hi();
            }
            MicroOp::IncInd => {
                unimplemented!()
            }
            MicroOp::DecInd => {
                unimplemented!()
            }
            MicroOp::MemReadImmLowStoreWord { reg } => {
                let addr = self.read_and_inc_pc();
                self.temp_reg_l = self.mem_read(addr)?;
                self.reg_write_word(reg, util::make_word(self.temp_reg_h, self.temp_reg_l));
            }
            MicroOp::ReadIO => {
                unimplemented!()
            }
            MicroOp::StoreIO => {
                unimplemented!()
            }
            MicroOp::LoadIOC => {
                unimplemented!()
            }
            MicroOp::StoreIOC => {
                unimplemented!()
            }
            MicroOp::LoadInc => {
                unimplemented!()
            }
            MicroOp::StoreInc => {
                unimplemented!()
            }
            MicroOp::LoadDec => {
                unimplemented!()
            }
            MicroOp::StoreDec => {
                unimplemented!()
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
            0x3E => self.enqueue(MicroOp::LoadImm(Reg8::A)),
            0x06 => self.enqueue(MicroOp::LoadImm(Reg8::B)),
            0x0E => self.enqueue(MicroOp::LoadImm(Reg8::C)),
            0x16 => self.enqueue(MicroOp::LoadImm(Reg8::D)),
            0x1E => self.enqueue(MicroOp::LoadImm(Reg8::E)),
            0x26 => self.enqueue(MicroOp::LoadImm(Reg8::H)),
            0x2E => self.enqueue(MicroOp::LoadImm(Reg8::L)),
            0x0A => self.enqueue(MicroOp::MemReadInd { reg: Reg8::A, addr_reg: Reg16::BC }),
            0x1A => self.enqueue(MicroOp::MemReadInd { reg: Reg8::A, addr_reg: Reg16::DE }),
            0x7E => self.enqueue(MicroOp::MemReadInd { reg: Reg8::A, addr_reg: Reg16::HL }),
            0x46 => self.enqueue(MicroOp::MemReadInd { reg: Reg8::B, addr_reg: Reg16::HL }),
            0x4E => self.enqueue(MicroOp::MemReadInd { reg: Reg8::C, addr_reg: Reg16::HL }),
            0x56 => self.enqueue(MicroOp::MemReadInd { reg: Reg8::D, addr_reg: Reg16::HL }),
            0x5E => self.enqueue(MicroOp::MemReadInd { reg: Reg8::E, addr_reg: Reg16::HL }),
            0x66 => self.enqueue(MicroOp::MemReadInd { reg: Reg8::H, addr_reg: Reg16::HL }),
            0x6E => self.enqueue(MicroOp::MemReadInd { reg: Reg8::L, addr_reg: Reg16::HL }),
            0xFA => {
                self.enqueue(MicroOp::TempLoadHighImm);
                self.enqueue(MicroOp::TempLoadLowImm);
                self.enqueue(MicroOp::MemReadIndTemp { reg: Reg8::A });
            }
            0x02 => self.enqueue(MicroOp::MemWriteInd { reg: Reg8::A, addr_reg: Reg16::BC }),
            0x12 => self.enqueue(MicroOp::MemWriteInd { reg: Reg8::A, addr_reg: Reg16::DE }),
            0x77 => self.enqueue(MicroOp::MemWriteInd { reg: Reg8::A, addr_reg: Reg16::HL }),
            0x70 => self.enqueue(MicroOp::MemWriteInd { reg: Reg8::B, addr_reg: Reg16::HL }),
            0x71 => self.enqueue(MicroOp::MemWriteInd { reg: Reg8::C, addr_reg: Reg16::HL }),
            0x72 => self.enqueue(MicroOp::MemWriteInd { reg: Reg8::D, addr_reg: Reg16::HL }),
            0x73 => self.enqueue(MicroOp::MemWriteInd { reg: Reg8::E, addr_reg: Reg16::HL }),
            0x74 => self.enqueue(MicroOp::MemWriteInd { reg: Reg8::H, addr_reg: Reg16::HL }),
            0x75 => self.enqueue(MicroOp::MemWriteInd { reg: Reg8::L, addr_reg: Reg16::HL }),
            0x36 => {
                self.enqueue(MicroOp::TempLoadHighImm);
                self.enqueue(MicroOp::MemWriteIndTempHigh { addr_reg: Reg16::HL });
            }
////                [0xEA, h, l] => Ok(Some(Op::StoreIndAImmediate(util::make_word(*h, *l)))),
            0xF0 => {
                self.enqueue(MicroOp::TempLoadHighImm);
                self.enqueue(MicroOp::ReadIO);
            }
            0xE0 => {
                self.enqueue(MicroOp::TempLoadHighImm);
                self.enqueue(MicroOp::StoreIO);
            }
            0xF2 => self.enqueue(MicroOp::LoadIOC),
            0xE2 => self.enqueue(MicroOp::StoreIOC),
            0x2A => self.enqueue(MicroOp::LoadInc),
            0x22 => self.enqueue(MicroOp::StoreInc),
            0x3A => self.enqueue(MicroOp::LoadDec),
            0x32 => self.enqueue(MicroOp::StoreDec),
            0x01 => {
                // TODO: Replace with half-register
                self.enqueue(MicroOp::TempLoadHighImm);
                self.enqueue(MicroOp::MemReadImmLowStoreWord { reg: Reg16::BC });
            }
            0x11 => {
                self.enqueue(MicroOp::TempLoadHighImm);
                self.enqueue(MicroOp::MemReadImmLowStoreWord { reg: Reg16::DE });
            }
            0x21 => {
                self.enqueue(MicroOp::TempLoadHighImm);
                self.enqueue(MicroOp::MemReadImmLowStoreWord { reg: Reg16::HL });
            }
            0x31 => {
                self.enqueue(MicroOp::TempLoadHighImm);
                self.enqueue(MicroOp::MemReadImmLowStoreWord { reg: Reg16::SP });
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
            0xC6 => self.enqueue(MicroOp::AddImm),
            0x86 => self.enqueue(MicroOp::AddInd),
            0x8F => self.add_carry_byte_reg(Reg8::A),
            0x88 => self.add_carry_byte_reg(Reg8::B),
            0x89 => self.add_carry_byte_reg(Reg8::C),
            0x8A => self.add_carry_byte_reg(Reg8::D),
            0x8B => self.add_carry_byte_reg(Reg8::E),
            0x8C => self.add_carry_byte_reg(Reg8::H),
            0x8D => self.add_carry_byte_reg(Reg8::L),
            0xCE => self.enqueue(MicroOp::AddCarryImm),
            0x8E => self.enqueue(MicroOp::AddCarryInd),
            0x97 => self.sub_byte_reg(Reg8::A),
            0x90 => self.sub_byte_reg(Reg8::B),
            0x91 => self.sub_byte_reg(Reg8::C),
            0x92 => self.sub_byte_reg(Reg8::D),
            0x93 => self.sub_byte_reg(Reg8::E),
            0x94 => self.sub_byte_reg(Reg8::H),
            0x95 => self.sub_byte_reg(Reg8::L),
            0xD6 => self.enqueue(MicroOp::SubImm),
            0x96 => self.enqueue(MicroOp::SubInd),
            0x9F => self.sub_carry_byte_reg(Reg8::A),
            0x98 => self.sub_carry_byte_reg(Reg8::B),
            0x99 => self.sub_carry_byte_reg(Reg8::C),
            0x9A => self.sub_carry_byte_reg(Reg8::D),
            0x9B => self.sub_carry_byte_reg(Reg8::E),
            0x9C => self.sub_carry_byte_reg(Reg8::H),
            0x9D => self.sub_carry_byte_reg(Reg8::L),
            0xDE => self.enqueue(MicroOp::SubCarryImm),
            0x9E => self.enqueue(MicroOp::SubCarryInd),
            0xA7 => self.and_byte_reg(Reg8::A),
            0xA0 => self.and_byte_reg(Reg8::B),
            0xA1 => self.and_byte_reg(Reg8::C),
            0xA2 => self.and_byte_reg(Reg8::D),
            0xA3 => self.and_byte_reg(Reg8::E),
            0xA4 => self.and_byte_reg(Reg8::H),
            0xA5 => self.and_byte_reg(Reg8::L),
            0xE6 => self.enqueue(MicroOp::AndImm),
            0xA6 => self.enqueue(MicroOp::AndInd),
            0xAF => self.xor_byte_reg(Reg8::A),
            0xA8 => self.xor_byte_reg(Reg8::B),
            0xA9 => self.xor_byte_reg(Reg8::C),
            0xAA => self.xor_byte_reg(Reg8::D),
            0xAB => self.xor_byte_reg(Reg8::E),
            0xAC => self.xor_byte_reg(Reg8::H),
            0xAD => self.xor_byte_reg(Reg8::L),
            0xEE => self.enqueue(MicroOp::XorImm),
            0xAE => self.enqueue(MicroOp::XorInd),
            0xB7 => self.or_byte_reg(Reg8::A),
            0xB0 => self.or_byte_reg(Reg8::B),
            0xB1 => self.or_byte_reg(Reg8::C),
            0xB2 => self.or_byte_reg(Reg8::D),
            0xB3 => self.or_byte_reg(Reg8::E),
            0xB4 => self.or_byte_reg(Reg8::H),
            0xB5 => self.or_byte_reg(Reg8::L),
            0xF6 => self.enqueue(MicroOp::OrImm),
            0xB6 => self.enqueue(MicroOp::OrInd),
            0xBF => self.cmp_byte_reg(Reg8::A),
            0xB8 => self.cmp_byte_reg(Reg8::B),
            0xB9 => self.cmp_byte_reg(Reg8::C),
            0xBA => self.cmp_byte_reg(Reg8::D),
            0xBB => self.cmp_byte_reg(Reg8::E),
            0xBC => self.cmp_byte_reg(Reg8::H),
            0xBD => self.cmp_byte_reg(Reg8::L),
            0xFE => self.enqueue(MicroOp::CmpImm),
            0xBE => self.enqueue(MicroOp::CmpInd),
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

    fn add_byte_tmp_hi(&mut self) {
        unimplemented!()
    }

    fn add_carry_byte_tmp_hi(&mut self) {
        unimplemented!()
    }

    fn sub_byte_tmp_hi(&mut self) {
        unimplemented!()
    }

    fn sub_carry_byte_tmp_hi(&mut self) {
        unimplemented!()
    }

    fn and_byte_tmp_hi(&mut self) {
        unimplemented!()
    }

    fn xor_byte_tmp_hi(&mut self) {
        unimplemented!()
    }

    fn or_byte_tmp_hi(&mut self) {
        unimplemented!()
    }

    fn cmp_byte_tmp_hi(&mut self) {
        unimplemented!()
    }

    fn inc_byte_tmp_hi(&mut self) {
        unimplemented!()
    }

    fn dec_byte_tmp_hi(&mut self) {
        unimplemented!()
    }

    fn reg_read_byte(&self, reg: Reg8) -> u8 {
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

    fn reg_read_word(&self, reg: Reg16) -> u16 {
        match reg {
            Reg16::AF => self.registers[0],
            Reg16::BC => self.registers[1],
            Reg16::DE => self.registers[2],
            Reg16::HL => self.registers[3],
            Reg16::SP => self.sp,
            Reg16::PC => self.pc,
        }
    }

    fn reg_write_byte(&mut self, reg: Reg8, byte: u8) {
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

    fn reg_write_word(&mut self, reg: Reg16, word: u16) {
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
    use std::ops::RangeInclusive;

    use crate::memory;
    use crate::memory::Addressable;
    use crate::memory::test_util::VecMemory;
    use crate::ops;

    use super::*;
    use crate::ops::Op;

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

        let mut cpu = Cpu::new(memory.clone());
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

        let mut cpu = Cpu::new(memory.clone());

        loop {
            println!("cpu.halted: {:?}", cpu.halted);
            println!("cpu.pc: {:?}", cpu.pc);
            println!("cpu.sp: {:?}", cpu.sp);
            println!("cpu.temp_reg_h: {:?}", cpu.temp_reg_h);
            println!("cpu.temp_reg_l: {:?}", cpu.temp_reg_l);
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
