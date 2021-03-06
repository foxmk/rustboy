use std::fmt;

use crate::{cpu, util};
use crate::cpu::{Flag, Reg16, Reg8};
use crate::interrupts::Interrupt;
use crate::util::Byte;

#[derive(Debug)]
pub(crate) enum Op {
    // 8-bit load/store
    Load(Reg8, Reg8),
    LoadImm(Reg8, u8),
    LoadInd(Reg8, Reg16),
    LoadIndAImm(u16),
    StoreInd(Reg16, Reg8),
    StoreImmediate(u8),
    StoreIndAImmediate(u16),
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
    Rlca,
    Rla,
    Rrca,
    Rra,
    Rlc(Reg8),
    RlcInd,
    Rl(Reg8),
    RlInd,
    Rrc(Reg8),
    RrcInd,
    Rr(Reg8),
    RrInd,
    Sla(Reg8),
    SlaInd,
    Swap(Reg8),
    SwapInd,
    Sra(Reg8),
    SraInd,
    Srl(Reg8),
    SrlInd,
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
    Reset(u16),
}

impl Into<Vec<u8>> for Op {
    fn into(self) -> Vec<u8> {
        match self {
            // 8-bit load/store
            Op::Load(Reg8::A, Reg8::A) => vec![0x7F],
            Op::Load(Reg8::A, Reg8::B) => vec![0x78],
            Op::Load(Reg8::A, Reg8::C) => vec![0x79],
            Op::Load(Reg8::A, Reg8::D) => vec![0x7A],
            Op::Load(Reg8::A, Reg8::E) => vec![0x7B],
            Op::Load(Reg8::A, Reg8::H) => vec![0x7C],
            Op::Load(Reg8::A, Reg8::L) => vec![0x7D],
            Op::Load(Reg8::B, Reg8::A) => vec![0x47],
            Op::Load(Reg8::B, Reg8::B) => vec![0x40],
            Op::Load(Reg8::B, Reg8::C) => vec![0x41],
            Op::Load(Reg8::B, Reg8::D) => vec![0x42],
            Op::Load(Reg8::B, Reg8::E) => vec![0x43],
            Op::Load(Reg8::B, Reg8::H) => vec![0x44],
            Op::Load(Reg8::B, Reg8::L) => vec![0x45],
            Op::Load(Reg8::C, Reg8::A) => vec![0x4F],
            Op::Load(Reg8::C, Reg8::B) => vec![0x48],
            Op::Load(Reg8::C, Reg8::C) => vec![0x49],
            Op::Load(Reg8::C, Reg8::D) => vec![0x4A],
            Op::Load(Reg8::C, Reg8::E) => vec![0x4B],
            Op::Load(Reg8::C, Reg8::H) => vec![0x4C],
            Op::Load(Reg8::C, Reg8::L) => vec![0x4D],
            Op::Load(Reg8::D, Reg8::A) => vec![0x57],
            Op::Load(Reg8::D, Reg8::B) => vec![0x50],
            Op::Load(Reg8::D, Reg8::C) => vec![0x51],
            Op::Load(Reg8::D, Reg8::D) => vec![0x52],
            Op::Load(Reg8::D, Reg8::E) => vec![0x53],
            Op::Load(Reg8::D, Reg8::H) => vec![0x54],
            Op::Load(Reg8::D, Reg8::L) => vec![0x55],
            Op::Load(Reg8::E, Reg8::A) => vec![0x5F],
            Op::Load(Reg8::E, Reg8::B) => vec![0x58],
            Op::Load(Reg8::E, Reg8::C) => vec![0x59],
            Op::Load(Reg8::E, Reg8::D) => vec![0x5A],
            Op::Load(Reg8::E, Reg8::E) => vec![0x5B],
            Op::Load(Reg8::E, Reg8::H) => vec![0x5C],
            Op::Load(Reg8::E, Reg8::L) => vec![0x5D],
            Op::Load(Reg8::H, Reg8::A) => vec![0x67],
            Op::Load(Reg8::H, Reg8::B) => vec![0x60],
            Op::Load(Reg8::H, Reg8::C) => vec![0x61],
            Op::Load(Reg8::H, Reg8::D) => vec![0x62],
            Op::Load(Reg8::H, Reg8::E) => vec![0x63],
            Op::Load(Reg8::H, Reg8::H) => vec![0x64],
            Op::Load(Reg8::H, Reg8::L) => vec![0x65],
            Op::Load(Reg8::L, Reg8::A) => vec![0x6F],
            Op::Load(Reg8::L, Reg8::B) => vec![0x68],
            Op::Load(Reg8::L, Reg8::C) => vec![0x69],
            Op::Load(Reg8::L, Reg8::D) => vec![0x6A],
            Op::Load(Reg8::L, Reg8::E) => vec![0x6B],
            Op::Load(Reg8::L, Reg8::H) => vec![0x6C],
            Op::Load(Reg8::L, Reg8::L) => vec![0x6D],
            Op::LoadImm(Reg8::A, byte) => vec![0x3E, byte],
            Op::LoadImm(Reg8::B, byte) => vec![0x06, byte],
            Op::LoadImm(Reg8::C, byte) => vec![0x0E, byte],
            Op::LoadImm(Reg8::D, byte) => vec![0x16, byte],
            Op::LoadImm(Reg8::E, byte) => vec![0x1E, byte],
            Op::LoadImm(Reg8::H, byte) => vec![0x26, byte],
            Op::LoadImm(Reg8::L, byte) => vec![0x2E, byte],
            // TODO: Split LoadInd Opcodes
            Op::LoadInd(Reg8::A, Reg16::BC) => vec![0x0A],
            Op::LoadInd(Reg8::A, Reg16::DE) => vec![0x1A],
            Op::LoadInd(Reg8::A, Reg16::HL) => vec![0x7E],
            Op::LoadInd(Reg8::B, Reg16::HL) => vec![0x46],
            Op::LoadInd(Reg8::C, Reg16::HL) => vec![0x4E],
            Op::LoadInd(Reg8::D, Reg16::HL) => vec![0x56],
            Op::LoadInd(Reg8::E, Reg16::HL) => vec![0x5E],
            Op::LoadInd(Reg8::H, Reg16::HL) => vec![0x66],
            Op::LoadInd(Reg8::L, Reg16::HL) => vec![0x6E],
            Op::LoadIndAImm(word) => {
                vec![0xFA, word.get_hi(), word.get_low()]
            }
            // TODO: Split StoreInd Opcodes
            Op::StoreInd(Reg16::BC, Reg8::A) => vec![0x02],
            Op::StoreInd(Reg16::DE, Reg8::A) => vec![0x12],
            Op::StoreInd(Reg16::HL, Reg8::A) => vec![0x77],
            Op::StoreInd(Reg16::HL, Reg8::B) => vec![0x70],
            Op::StoreInd(Reg16::HL, Reg8::C) => vec![0x71],
            Op::StoreInd(Reg16::HL, Reg8::D) => vec![0x72],
            Op::StoreInd(Reg16::HL, Reg8::E) => vec![0x73],
            Op::StoreInd(Reg16::HL, Reg8::H) => vec![0x74],
            Op::StoreInd(Reg16::HL, Reg8::L) => vec![0x75],
            Op::StoreImmediate(byte) => vec![0x36, byte],
            Op::StoreIndAImmediate(word) => {
                vec![0xEA, word.get_hi(), word.get_low()]
            }
            Op::LoadIO(offset) => vec![0xF0, offset],
            Op::StoreIO(offset) => vec![0xE0, offset],
            Op::LoadIOC => vec![0xF2],
            Op::StoreIOC => vec![0xE2],
            Op::LoadInc => vec![0x2A],
            Op::StoreInc => vec![0x22],
            Op::LoadDec => vec![0x3A],
            Op::StoreDec => vec![0x32],
            // 16-bit load/store
            Op::Load16(Reg16::BC, word) => {
                vec![0x01, word.get_hi(), word.get_low()]
            }
            Op::Load16(Reg16::DE, word) => {
                vec![0x11, word.get_hi(), word.get_low()]
            }
            Op::Load16(Reg16::HL, word) => {
                vec![0x21, word.get_hi(), word.get_low()]
            }
            Op::Load16(Reg16::SP, word) => {
                vec![0x31, word.get_hi(), word.get_low()]
            }
            Op::LoadSP => vec![0xF9],
            Op::Push(Reg16::BC) => vec![0xC5],
            Op::Push(Reg16::DE) => vec![0xD5],
            Op::Push(Reg16::HL) => vec![0xE5],
            Op::Push(Reg16::AF) => vec![0xF5],
            Op::Pop(Reg16::BC) => vec![0xC1],
            Op::Pop(Reg16::DE) => vec![0xD1],
            Op::Pop(Reg16::HL) => vec![0xE1],
            Op::Pop(Reg16::AF) => vec![0xF1],
            // 8-bit arithmetic
            Op::Add(Reg8::A) => vec![0x87],
            Op::Add(Reg8::B) => vec![0x80],
            Op::Add(Reg8::C) => vec![0x81],
            Op::Add(Reg8::D) => vec![0x82],
            Op::Add(Reg8::E) => vec![0x83],
            Op::Add(Reg8::H) => vec![0x84],
            Op::Add(Reg8::L) => vec![0x85],
            Op::AddImmediate(byte) => vec![0xC6, byte],
            Op::AddIndHL => vec![0x86],
            Op::AddCarry(Reg8::A) => vec![0x8F],
            Op::AddCarry(Reg8::B) => vec![0x88],
            Op::AddCarry(Reg8::C) => vec![0x89],
            Op::AddCarry(Reg8::D) => vec![0x8A],
            Op::AddCarry(Reg8::E) => vec![0x8B],
            Op::AddCarry(Reg8::H) => vec![0x8C],
            Op::AddCarry(Reg8::L) => vec![0x8D],
            Op::AddCarryImmediate(byte) => vec![0xCE, byte],
            Op::AddCarryIndHL => vec![0x8E],
            Op::Sub(Reg8::A) => vec![0x97],
            Op::Sub(Reg8::B) => vec![0x90],
            Op::Sub(Reg8::C) => vec![0x91],
            Op::Sub(Reg8::D) => vec![0x92],
            Op::Sub(Reg8::E) => vec![0x93],
            Op::Sub(Reg8::H) => vec![0x94],
            Op::Sub(Reg8::L) => vec![0x95],
            Op::SubImmediate(byte) => vec![0xD6, byte],
            Op::SubIndHL => vec![0x96],
            Op::SubCarry(Reg8::A) => vec![0x9F],
            Op::SubCarry(Reg8::B) => vec![0x98],
            Op::SubCarry(Reg8::C) => vec![0x99],
            Op::SubCarry(Reg8::D) => vec![0x9A],
            Op::SubCarry(Reg8::E) => vec![0x9B],
            Op::SubCarry(Reg8::H) => vec![0x9C],
            Op::SubCarry(Reg8::L) => vec![0x9D],
            Op::SubCarryImmediate(byte) => vec![0xDE, byte],
            Op::SubCarryIndHL => vec![0x9E],
            Op::And(Reg8::A) => vec![0xA7],
            Op::And(Reg8::B) => vec![0xA0],
            Op::And(Reg8::C) => vec![0xA1],
            Op::And(Reg8::D) => vec![0xA2],
            Op::And(Reg8::E) => vec![0xA3],
            Op::And(Reg8::H) => vec![0xA4],
            Op::And(Reg8::L) => vec![0xA5],
            Op::AndImmediate(byte) => vec![0xE6, byte],
            Op::AndIndHL => vec![0xA6],
            Op::Xor(Reg8::A) => vec![0xAF],
            Op::Xor(Reg8::B) => vec![0xA8],
            Op::Xor(Reg8::C) => vec![0xA9],
            Op::Xor(Reg8::D) => vec![0xAA],
            Op::Xor(Reg8::E) => vec![0xAB],
            Op::Xor(Reg8::H) => vec![0xAC],
            Op::Xor(Reg8::L) => vec![0xAD],
            Op::XorImmediate(byte) => vec![0xEE, byte],
            Op::XorIndHL => vec![0xAE],
            Op::Or(Reg8::A) => vec![0xB7],
            Op::Or(Reg8::B) => vec![0xB0],
            Op::Or(Reg8::C) => vec![0xB1],
            Op::Or(Reg8::D) => vec![0xB2],
            Op::Or(Reg8::E) => vec![0xB3],
            Op::Or(Reg8::H) => vec![0xB4],
            Op::Or(Reg8::L) => vec![0xB5],
            Op::OrImmediate(byte) => vec![0xF6, byte],
            Op::OrIndHL => vec![0xB6],
            Op::Cmp(Reg8::A) => vec![0xBF],
            Op::Cmp(Reg8::B) => vec![0xB8],
            Op::Cmp(Reg8::C) => vec![0xB9],
            Op::Cmp(Reg8::D) => vec![0xBA],
            Op::Cmp(Reg8::E) => vec![0xBB],
            Op::Cmp(Reg8::H) => vec![0xBC],
            Op::Cmp(Reg8::L) => vec![0xBD],
            Op::CmpImmediate(byte) => vec![0xFE, byte],
            Op::CmpIndHL => vec![0xBE],
            Op::Inc(Reg8::A) => vec![0x3C],
            Op::Inc(Reg8::B) => vec![0x04],
            Op::Inc(Reg8::C) => vec![0x0C],
            Op::Inc(Reg8::D) => vec![0x14],
            Op::Inc(Reg8::E) => vec![0x1C],
            Op::Inc(Reg8::H) => vec![0x24],
            Op::Inc(Reg8::L) => vec![0x2C],
            Op::IncIndHL => vec![0x34],
            Op::Dec(Reg8::A) => vec![0x3D],
            Op::Dec(Reg8::B) => vec![0x05],
            Op::Dec(Reg8::C) => vec![0x0D],
            Op::Dec(Reg8::D) => vec![0x15],
            Op::Dec(Reg8::E) => vec![0x1D],
            Op::Dec(Reg8::H) => vec![0x25],
            Op::Dec(Reg8::L) => vec![0x2D],
            Op::DecIndHL => vec![0x35],
            Op::Daa => vec![0x27],
            Op::Cpl => vec![0x2F],
            // 16-bit arithmetic
            Op::Add16HL(Reg16::BC) => vec![0x09],
            Op::Add16HL(Reg16::DE) => vec![0x19],
            Op::Add16HL(Reg16::HL) => vec![0x29],
            Op::Add16HL(Reg16::SP) => vec![0x39],
            Op::Inc16(Reg16::BC) => vec![0x03],
            Op::Inc16(Reg16::DE) => vec![0x13],
            Op::Inc16(Reg16::HL) => vec![0x23],
            Op::Inc16(Reg16::SP) => vec![0x33],
            Op::Dec16(Reg16::BC) => vec![0x0B],
            Op::Dec16(Reg16::DE) => vec![0x1B],
            Op::Dec16(Reg16::HL) => vec![0x2B],
            Op::Dec16(Reg16::SP) => vec![0x3B],
            Op::AddSignedSP(offset) => vec![0xE8, offset],
            Op::LoadSigned(offset) => vec![0xF8, offset],
            // TODO: Implement conversion to bytes
            // Rotate and shift
            // Op::Rlca => vec![0x00],
            // Op::Rla => vec![0x00],
            // Op::Rrca => vec![0x00],
            // Op::Rra => vec![0x00],
            // Op::Rlc(Reg8::A) => vec![0x00],
            // Op::Rlc(Reg8::B) => vec![0x00],
            // Op::Rlc(Reg8::C) => vec![0x00],
            // Op::Rlc(Reg8::D) => vec![0x00],
            // Op::Rlc(Reg8::E) => vec![0x00],
            // Op::Rlc(Reg8::H) => vec![0x00],
            // Op::Rlc(Reg8::L) => vec![0x00],
            // Op::RlcInd => vec![0x00],
            // Op::Rl(Reg8::A) => vec![0x00],
            // Op::Rl(Reg8::B) => vec![0x00],
            // Op::Rl(Reg8::C) => vec![0x00],
            // Op::Rl(Reg8::D) => vec![0x00],
            // Op::Rl(Reg8::E) => vec![0x00],
            // Op::Rl(Reg8::H) => vec![0x00],
            // Op::Rl(Reg8::L) => vec![0x00],
            // Op::RlInd => vec![0x00],
            // Op::Rrc(Reg8::A) => vec![0x00],
            // Op::Rrc(Reg8::B) => vec![0x00],
            // Op::Rrc(Reg8::C) => vec![0x00],
            // Op::Rrc(Reg8::D) => vec![0x00],
            // Op::Rrc(Reg8::E) => vec![0x00],
            // Op::Rrc(Reg8::H) => vec![0x00],
            // Op::Rrc(Reg8::L) => vec![0x00],
            // Op::RrcInd => vec![0x00],
            // Op::Rr(Reg8::A) => vec![0x00],
            // Op::Rr(Reg8::B) => vec![0x00],
            // Op::Rr(Reg8::C) => vec![0x00],
            // Op::Rr(Reg8::D) => vec![0x00],
            // Op::Rr(Reg8::E) => vec![0x00],
            // Op::Rr(Reg8::H) => vec![0x00],
            // Op::Rr(Reg8::L) => vec![0x00],
            // Op::RrInd => vec![0x00],
            // Op::Sla(Reg8::A) => vec![0x00],
            // Op::Sla(Reg8::B) => vec![0x00],
            // Op::Sla(Reg8::C) => vec![0x00],
            // Op::Sla(Reg8::D) => vec![0x00],
            // Op::Sla(Reg8::E) => vec![0x00],
            // Op::Sla(Reg8::H) => vec![0x00],
            // Op::Sla(Reg8::L) => vec![0x00],
            // Op::SlaInd => vec![0x00],
            // Op::Swap(Reg8::A) => vec![0x00],
            // Op::Swap(Reg8::B) => vec![0x00],
            // Op::Swap(Reg8::C) => vec![0x00],
            // Op::Swap(Reg8::D) => vec![0x00],
            // Op::Swap(Reg8::E) => vec![0x00],
            // Op::Swap(Reg8::H) => vec![0x00],
            // Op::Swap(Reg8::L) => vec![0x00],
            // Op::SwapInd => vec![0x00],
            // Op::Sra(Reg8::A) => vec![0x00],
            // Op::Sra(Reg8::B) => vec![0x00],
            // Op::Sra(Reg8::C) => vec![0x00],
            // Op::Sra(Reg8::D) => vec![0x00],
            // Op::Sra(Reg8::E) => vec![0x00],
            // Op::Sra(Reg8::H) => vec![0x00],
            // Op::Sra(Reg8::L) => vec![0x00],
            // Op::SraInd => vec![0x00],
            // Op::Srl(Reg8::A) => vec![0x00],
            // Op::Srl(Reg8::B) => vec![0x00],
            // Op::Srl(Reg8::C) => vec![0x00],
            // Op::Srl(Reg8::D) => vec![0x00],
            // Op::Srl(Reg8::E) => vec![0x00],
            // Op::Srl(Reg8::H) => vec![0x00],
            // Op::Srl(Reg8::L) => vec![0x00],
            // Op::SrlInd => vec![0x00],
            // Single bit
            //            Op::Bit(0, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Bit(0, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Bit(0, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Bit(0, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Bit(0, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Bit(0, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Bit(0, Reg8::L) => vec![0xCB, 0x00],
            //            Op::Bit(1, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Bit(1, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Bit(1, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Bit(1, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Bit(1, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Bit(1, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Bit(1, Reg8::L) => vec![0xCB, 0x00],
            //            Op::Bit(2, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Bit(2, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Bit(2, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Bit(2, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Bit(2, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Bit(2, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Bit(2, Reg8::L) => vec![0xCB, 0x00],
            //            Op::Bit(3, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Bit(3, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Bit(3, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Bit(3, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Bit(3, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Bit(3, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Bit(3, Reg8::L) => vec![0xCB, 0x00],
            //            Op::Bit(4, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Bit(4, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Bit(4, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Bit(4, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Bit(4, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Bit(4, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Bit(4, Reg8::L) => vec![0xCB, 0x00],
            //            Op::Bit(5, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Bit(5, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Bit(5, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Bit(5, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Bit(5, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Bit(5, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Bit(5, Reg8::L) => vec![0xCB, 0x00],
            //            Op::Bit(6, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Bit(6, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Bit(6, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Bit(6, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Bit(6, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Bit(6, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Bit(6, Reg8::L) => vec![0xCB, 0x00],
            //            Op::Bit(7, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Bit(7, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Bit(7, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Bit(7, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Bit(7, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Bit(7, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Bit(7, Reg8::L) => vec![0xCB, 0x00],
            //            Op::BitHL(0) => vec![0xCB, 0x00],
            //            Op::BitHL(1) => vec![0xCB, 0x00],
            //            Op::BitHL(2) => vec![0xCB, 0x00],
            //            Op::BitHL(3) => vec![0xCB, 0x00],
            //            Op::BitHL(4) => vec![0xCB, 0x00],
            //            Op::BitHL(5) => vec![0xCB, 0x00],
            //            Op::BitHL(6) => vec![0xCB, 0x00],
            //            Op::BitHL(7) => vec![0xCB, 0x00],
            //            Op::Set(0, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Set(0, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Set(0, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Set(0, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Set(0, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Set(0, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Set(0, Reg8::L) => vec![0xCB, 0x00],
            //            Op::Set(1, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Set(1, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Set(1, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Set(1, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Set(1, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Set(1, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Set(1, Reg8::L) => vec![0xCB, 0x00],
            //            Op::Set(2, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Set(2, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Set(2, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Set(2, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Set(2, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Set(2, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Set(2, Reg8::L) => vec![0xCB, 0x00],
            //            Op::Set(3, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Set(3, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Set(3, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Set(3, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Set(3, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Set(3, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Set(3, Reg8::L) => vec![0xCB, 0x00],
            //            Op::Set(4, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Set(4, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Set(4, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Set(4, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Set(4, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Set(4, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Set(4, Reg8::L) => vec![0xCB, 0x00],
            //            Op::Set(5, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Set(5, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Set(5, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Set(5, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Set(5, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Set(5, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Set(5, Reg8::L) => vec![0xCB, 0x00],
            //            Op::Set(6, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Set(6, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Set(6, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Set(6, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Set(6, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Set(6, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Set(6, Reg8::L) => vec![0xCB, 0x00],
            //            Op::Set(7, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Set(7, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Set(7, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Set(7, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Set(7, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Set(7, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Set(7, Reg8::L) => vec![0xCB, 0x00],
            //            Op::SetHL(0) => vec![0xCB, 0x00],
            //            Op::SetHL(1) => vec![0xCB, 0x00],
            //            Op::SetHL(2) => vec![0xCB, 0x00],
            //            Op::SetHL(3) => vec![0xCB, 0x00],
            //            Op::SetHL(4) => vec![0xCB, 0x00],
            //            Op::SetHL(5) => vec![0xCB, 0x00],
            //            Op::SetHL(6) => vec![0xCB, 0x00],
            //            Op::SetHL(7) => vec![0xCB, 0x00],
            //            Op::Res(0, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Res(0, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Res(0, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Res(0, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Res(0, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Res(0, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Res(0, Reg8::L) => vec![0xCB, 0x00],
            //            Op::Res(1, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Res(1, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Res(1, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Res(1, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Res(1, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Res(1, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Res(1, Reg8::L) => vec![0xCB, 0x00],
            //            Op::Res(2, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Res(2, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Res(2, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Res(2, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Res(2, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Res(2, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Res(2, Reg8::L) => vec![0xCB, 0x00],
            //            Op::Res(3, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Res(3, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Res(3, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Res(3, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Res(3, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Res(3, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Res(3, Reg8::L) => vec![0xCB, 0x00],
            //            Op::Res(4, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Res(4, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Res(4, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Res(4, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Res(4, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Res(4, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Res(4, Reg8::L) => vec![0xCB, 0x00],
            //            Op::Res(5, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Res(5, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Res(5, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Res(5, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Res(5, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Res(5, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Res(5, Reg8::L) => vec![0xCB, 0x00],
            //            Op::Res(6, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Res(6, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Res(6, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Res(6, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Res(6, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Res(6, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Res(6, Reg8::L) => vec![0xCB, 0x00],
            //            Op::Res(7, Reg8::A) => vec![0xCB, 0x00],
            //            Op::Res(7, Reg8::B) => vec![0xCB, 0x00],
            //            Op::Res(7, Reg8::C) => vec![0xCB, 0x00],
            //            Op::Res(7, Reg8::D) => vec![0xCB, 0x00],
            //            Op::Res(7, Reg8::E) => vec![0xCB, 0x00],
            //            Op::Res(7, Reg8::H) => vec![0xCB, 0x00],
            //            Op::Res(7, Reg8::L) => vec![0xCB, 0x00],
            //            Op::ResHL(0) => vec![0xCB, 0x00],
            //            Op::ResHL(1) => vec![0xCB, 0x00],
            //            Op::ResHL(2) => vec![0xCB, 0x00],
            //            Op::ResHL(3) => vec![0xCB, 0x00],
            //            Op::ResHL(4) => vec![0xCB, 0x00],
            //            Op::ResHL(5) => vec![0xCB, 0x00],
            //            Op::ResHL(6) => vec![0xCB, 0x00],
            //            Op::ResHL(7) => vec![0xCB, 0x00],
            // Control
            Op::Ccf => vec![0x3F],
            Op::Scf => vec![0x37],
            Op::Nop => vec![0x00],
            Op::Halt => vec![0x76],
            Op::Stop => vec![0x10, 0x00],
            Op::Di => vec![0xF3],
            Op::Ei => vec![0xFB],
            // Jump
            Op::Jump(n) => vec![0xC3, n.get_hi(), n.get_low()],
            Op::JumpInd => vec![0xE9],
            //            Op::JumpCond(Flag::Z, n) => vec![0x00, n.get_hi(), util::get_low_byte(n)],
            //            Op::JumpCond(Flag::N, n) => vec![0x00, n.get_hi(), util::get_low_byte(n)],
            //            Op::JumpCond(Flag::H, n) => vec![0x00, n.get_hi(), util::get_low_byte(n)],
            //            Op::JumpCond(Flag::C, n) => vec![0x00, n.get_hi(), util::get_low_byte(n)],
            Op::JumpRel(offset) => vec![0x18, offset],
            //            Op::JumpRelCond(Flag::Z, offset) => vec![0x00, offset],
            //            Op::JumpRelCond(Flag::N, offset) => vec![0x00, offset],
            //            Op::JumpRelCond(Flag::H, offset) => vec![0x00, offset],
            //            Op::JumpRelCond(Flag::C, offset) => vec![0x00, offset],
            Op::Call(n) => vec![0xCD, n.get_hi(), n.get_low()],
            //            Op::CallCond(Flag::Z, n) => vec![0x00, n.get_hi(), util::get_low_byte(n)],
            //            Op::CallCond(Flag::N, n) => vec![0x00, n.get_hi(), util::get_low_byte(n)],
            //            Op::CallCond(Flag::H, n) => vec![0x00, n.get_hi(), util::get_low_byte(n)],
            //            Op::CallCond(Flag::C, n) => vec![0x00, n.get_hi(), util::get_low_byte(n)],
            Op::Ret => vec![0xC9],
            //            Op::RetCond(Flag::Z) => vec![0x00],
            //            Op::RetCond(Flag::N) => vec![0x00],
            //            Op::RetCond(Flag::H) => vec![0x00],
            //            Op::RetCond(Flag::C) => vec![0x00],
            Op::RetEnable => vec![0x00],
            Op::Reset(0x0000) => vec![0xC7],
            Op::Reset(0x0008) => vec![0xCF],
            Op::Reset(0x0010) => vec![0xD7],
            Op::Reset(0x0018) => vec![0xDF],
            Op::Reset(0x0020) => vec![0xE7],
            Op::Reset(0x0028) => vec![0xEF],
            Op::Reset(0x0030) => vec![0xF7],
            Op::Reset(0x0038) => vec![0xFF],
            other => unimplemented!("Invalid opcode: {:?}", other),
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            // 8-bit load/store
            Op::Load(dest, src) => write!(f, "LD {:?}, {:?}", dest, src),
            Op::LoadImm(dest, byte) => write!(f, "LD {:?}, ${:#04x}", dest, byte),
            Op::LoadInd(dest, src) => write!(f, "LD {:?}, ({:?})", dest, src),
            Op::LoadIndAImm(src) => write!(f, "LD A, (${:#04x})", src),
            Op::StoreInd(dest, src) => write!(f, "LD ({:?}), {:?}", dest, src),
            Op::StoreImmediate(byte) => write!(f, "LD (HL), ${:#04x}", byte),
            Op::StoreIndAImmediate(dest) => write!(f, "LD (${:#06x}), A", dest),
            Op::LoadIO(offset) => write!(f, "LD A, ($FF00+${:#04x})", offset),
            Op::StoreIO(offset) => write!(f, "LD ($FF00+${:#04x}), A", offset),
            Op::LoadIOC => write!(f, "LD A, ($FF00+C)"),
            Op::StoreIOC => write!(f, "LD ($FF00+C), A"),
            Op::LoadInc => write!(f, "LD A, (HL+)"),
            Op::StoreInc => write!(f, "LD (HL+), A"),
            Op::LoadDec => write!(f, "LD A, (HL-)"),
            Op::StoreDec => write!(f, "LD (HL-), A"),
            // 16-bit load/store
            Op::Load16(dest, src) => write!(f, "LD {:?}, ${:#06x}", dest, src),
            Op::LoadSP => write!(f, "LD SP, HL"),
            Op::Push(dest) => write!(f, "PUSH {:?}", dest),
            Op::Pop(dest) => write!(f, "POP {:?}", dest),
            // 8-bit arithmetic
            Op::Add(reg) => write!(f, "ADD A, {:?}", reg),
            Op::AddImmediate(byte) => write!(f, "ADD A, ${:#04x}", byte),
            Op::AddIndHL => write!(f, "ADD A, (HL)"),
            Op::AddCarry(reg) => write!(f, "ADC A, {:?}", reg),
            Op::AddCarryImmediate(byte) => write!(f, "ADC A, ${:#04x}", byte),
            Op::AddCarryIndHL => write!(f, "ADC A, (HL)"),
            Op::Sub(reg) => write!(f, "SUB A, {:?}", reg),
            Op::SubImmediate(byte) => write!(f, "SUB A, ${:#04x}", byte),
            Op::SubIndHL => write!(f, "SUB A, (HL)"),
            Op::SubCarry(reg) => write!(f, "SBC A, {:?}", reg),
            Op::SubCarryImmediate(byte) => write!(f, "SDB A, ${:#04x}", byte),
            Op::SubCarryIndHL => write!(f, "SBC A, (HL)"),
            Op::And(reg) => write!(f, "AND A, {:?}", reg),
            Op::AndImmediate(byte) => write!(f, "AND A, ${:#04x}", byte),
            Op::AndIndHL => write!(f, "AND A, (HL)"),
            Op::Xor(reg) => write!(f, "XOR A, {:?}", reg),
            Op::XorImmediate(byte) => write!(f, "XOR A, ${:#04x}", byte),
            Op::XorIndHL => write!(f, "XOR A, (HL)"),
            Op::Or(reg) => write!(f, "OR A, {:?}", reg),
            Op::OrImmediate(byte) => write!(f, "OR A, ${:#04x}", byte),
            Op::OrIndHL => write!(f, "OR A, (HL)"),
            Op::Cmp(reg) => write!(f, "CMP A, {:?}", reg),
            Op::CmpImmediate(byte) => write!(f, " A, ${:#04x}", byte),
            Op::CmpIndHL => write!(f, "CMP A, (HL)"),
            Op::Inc(reg) => write!(f, "INC {:?}", reg),
            Op::IncIndHL => write!(f, "INC (HL)"),
            Op::Dec(reg) => write!(f, "DEC {:?}", reg),
            Op::DecIndHL => write!(f, "DEC (HL)"),
            Op::Daa => write!(f, "DAA"),
            Op::Cpl => write!(f, "CPL"),
            // 16-bit arithmetic
            Op::Add16HL(reg) => write!(f, "ADD HL, {:?}", reg),
            Op::Inc16(reg) => write!(f, "INC {:?}", reg),
            Op::Dec16(reg) => write!(f, "DEC {:?}", reg),
            Op::AddSignedSP(byte) => write!(f, "ADD SP, ${:#04x}", byte),
            Op::LoadSigned(byte) => write!(f, "LD HL, SP+${:#04x}", byte),
            // Rotate/shift
            Op::Rlca => write!(f, "RLCA"),
            Op::Rla => write!(f, "RLA"),
            Op::Rrca => write!(f, "RRCA"),
            Op::Rra => write!(f, "RRA"),
            Op::Rlc(reg) => write!(f, "RLC {:?}", reg),
            Op::RlcInd => write!(f, "RLC (HL)"),
            Op::Rl(reg) => write!(f, "RL {:?}", reg),
            Op::RlInd => write!(f, "RL (HL)"),
            Op::Rrc(reg) => write!(f, "RRC {:?}", reg),
            Op::RrcInd => write!(f, "RRC (HL)"),
            Op::Rr(reg) => write!(f, "RR {:?}", reg),
            Op::RrInd => write!(f, "RR (HL)"),
            Op::Sla(reg) => write!(f, "SLA {:?}", reg),
            Op::SlaInd => write!(f, "SLA (HL)"),
            Op::Swap(reg) => write!(f, "SWAP {:?}", reg),
            Op::SwapInd => write!(f, "SWAP (HL)"),
            Op::Sra(reg) => write!(f, "SRA {:?}", reg),
            Op::SraInd => write!(f, "SRA (HL)"),
            Op::Srl(reg) => write!(f, "SRL {:?}", reg),
            Op::SrlInd => write!(f, "SRL (HL)"),
            // Single bit
            Op::Bit(n, reg) => write!(f, "BIT {}, {:?}", n, reg),
            Op::BitHL(n) => write!(f, "BIT {}, (HL)", n),
            Op::Set(n, reg) => write!(f, "SET {}, {:?}", n, reg),
            Op::SetHL(n) => write!(f, "SET {}, (HL)", n),
            Op::Res(n, reg) => write!(f, "RES {}, {:?}", n, reg),
            Op::ResHL(n) => write!(f, "RES {}, (HL)", n),
            // Control
            Op::Ccf => write!(f, "CCF"),
            Op::Scf => write!(f, "SCF"),
            Op::Nop => write!(f, "NOP"),
            Op::Halt => write!(f, "HLT"),
            Op::Stop => write!(f, "STOP"),
            Op::Di => write!(f, "DI"),
            Op::Ei => write!(f, "EI"),
            // Jump
            Op::Jump(n) => write!(f, "JP ${:#06x}", n),
            Op::JumpInd => write!(f, "JP HL"),
            Op::JumpCond(flag, n) => write!(f, "JP {:?}, ${:#06x}", flag, n),
            Op::JumpRel(n) => write!(f, "JR PC+${:#04x}", n),
            Op::JumpRelCond(flag, n) => write!(f, "JR {:?}, PC+${:#04x}", flag, n),
            Op::Call(n) => write!(f, "CALL ${:#06x}", n),
            Op::CallCond(flag, n) => write!(f, "CALL {:?}, ${:#06x}", flag, n),
            Op::Ret => write!(f, "RET"),
            Op::RetCond(flag) => write!(f, "RET {:?}", flag),
            Op::RetEnable => write!(f, "RETI"),
            Op::Reset(n) => write!(f, "RST {}", n),
        }
    }
}