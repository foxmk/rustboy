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
enum Reg16 { AF, BC, DE, HL, SP, PC }

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
            Op::LoadIndAImm(word) => vec![0xFA, util::get_high_byte(word), util::get_low_byte(word)],
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
            Op::StoreIndAImmediate(word) => vec![0xEA, util::get_high_byte(word), util::get_low_byte(word)],
            Op::LoadIO(offset) => vec![0xF0, offset],
            Op::StoreIO(offset) => vec![0xE0, offset],
            Op::LoadIOC => vec![0xF2],
            Op::StoreIOC => vec![0xE2],
            Op::LoadInc => vec![0x2A],
            Op::StoreInc => vec![0x22],
            Op::LoadDec => vec![0x3A],
            Op::StoreDec => vec![0x32],
            // 16-bit load/store
            Op::Load16(Reg16::BC, word) => vec![0x01, util::get_high_byte(word), util::get_low_byte(word)],
            Op::Load16(Reg16::DE, word) => vec![0x11, util::get_high_byte(word), util::get_low_byte(word)],
            Op::Load16(Reg16::HL, word) => vec![0x21, util::get_high_byte(word), util::get_low_byte(word)],
            Op::Load16(Reg16::SP, word) => vec![0x31, util::get_high_byte(word), util::get_low_byte(word)],
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
            Op::Jump(n) => vec![0xC3, util::get_high_byte(n), util::get_low_byte(n)],
            Op::JumpInd => vec![0xE9],
//            Op::JumpCond(Flag::Z, n) => vec![0x00, util::get_high_byte(n), util::get_low_byte(n)],
//            Op::JumpCond(Flag::N, n) => vec![0x00, util::get_high_byte(n), util::get_low_byte(n)],
//            Op::JumpCond(Flag::H, n) => vec![0x00, util::get_high_byte(n), util::get_low_byte(n)],
//            Op::JumpCond(Flag::C, n) => vec![0x00, util::get_high_byte(n), util::get_low_byte(n)],
            Op::JumpRel(offset) => vec![0x18, offset],
//            Op::JumpRelCond(Flag::Z, offset) => vec![0x00, offset],
//            Op::JumpRelCond(Flag::N, offset) => vec![0x00, offset],
//            Op::JumpRelCond(Flag::H, offset) => vec![0x00, offset],
//            Op::JumpRelCond(Flag::C, offset) => vec![0x00, offset],
            Op::Call(n) => vec![0xCD, util::get_high_byte(n), util::get_low_byte(n)],
//            Op::CallCond(Flag::Z, n) => vec![0x00, util::get_high_byte(n), util::get_low_byte(n)],
//            Op::CallCond(Flag::N, n) => vec![0x00, util::get_high_byte(n), util::get_low_byte(n)],
//            Op::CallCond(Flag::H, n) => vec![0x00, util::get_high_byte(n), util::get_low_byte(n)],
//            Op::CallCond(Flag::C, n) => vec![0x00, util::get_high_byte(n), util::get_low_byte(n)],
            Op::Ret => vec![0xC9],
//            Op::RetCond(Flag::Z) => vec![0x00],
//            Op::RetCond(Flag::N) => vec![0x00],
//            Op::RetCond(Flag::H) => vec![0x00],
//            Op::RetCond(Flag::C) => vec![0x00],
            Op::RetEnable => vec![0x00],
            Op::Reset(0x00) => vec![0xC7],
            Op::Reset(0x08) => vec![0xCF],
            Op::Reset(0x10) => vec![0xD7],
            Op::Reset(0x18) => vec![0xDF],
            Op::Reset(0x20) => vec![0xE7],
            Op::Reset(0x28) => vec![0xEF],
            Op::Reset(0x30) => vec![0xF7],
            Op::Reset(0x38) => vec![0xFF],
            other => unimplemented!("Invalid opcode: {}", other)
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

            if util::get_bit(interrupt_enable_flags, 0) && util::get_bit(interrupt_flags, 0) {
                // VBlank
                self.sp = self.pc;
                self.pc = 0x0040;
            }
        }

        Ok(true)
    }

    fn decode(bytes: &[u8]) -> Result<Option<Op>, Error> {
        match bytes {
            [0x7F] => Ok(Some(Op::Load(Reg8::A, Reg8::A))),
            [0x78] => Ok(Some(Op::Load(Reg8::A, Reg8::B))),
            [0x79] => Ok(Some(Op::Load(Reg8::A, Reg8::C))),
            [0x7A] => Ok(Some(Op::Load(Reg8::A, Reg8::D))),
            [0x7B] => Ok(Some(Op::Load(Reg8::A, Reg8::E))),
            [0x7C] => Ok(Some(Op::Load(Reg8::A, Reg8::H))),
            [0x7D] => Ok(Some(Op::Load(Reg8::A, Reg8::L))),
            [0x47] => Ok(Some(Op::Load(Reg8::B, Reg8::A))),
            [0x40] => Ok(Some(Op::Load(Reg8::B, Reg8::B))),
            [0x41] => Ok(Some(Op::Load(Reg8::B, Reg8::C))),
            [0x42] => Ok(Some(Op::Load(Reg8::B, Reg8::D))),
            [0x43] => Ok(Some(Op::Load(Reg8::B, Reg8::E))),
            [0x44] => Ok(Some(Op::Load(Reg8::B, Reg8::H))),
            [0x45] => Ok(Some(Op::Load(Reg8::B, Reg8::L))),
            [0x4F] => Ok(Some(Op::Load(Reg8::C, Reg8::A))),
            [0x48] => Ok(Some(Op::Load(Reg8::C, Reg8::B))),
            [0x49] => Ok(Some(Op::Load(Reg8::C, Reg8::C))),
            [0x4A] => Ok(Some(Op::Load(Reg8::C, Reg8::D))),
            [0x4B] => Ok(Some(Op::Load(Reg8::C, Reg8::E))),
            [0x4C] => Ok(Some(Op::Load(Reg8::C, Reg8::H))),
            [0x4D] => Ok(Some(Op::Load(Reg8::C, Reg8::L))),
            [0x57] => Ok(Some(Op::Load(Reg8::D, Reg8::A))),
            [0x50] => Ok(Some(Op::Load(Reg8::D, Reg8::B))),
            [0x51] => Ok(Some(Op::Load(Reg8::D, Reg8::C))),
            [0x52] => Ok(Some(Op::Load(Reg8::D, Reg8::D))),
            [0x53] => Ok(Some(Op::Load(Reg8::D, Reg8::E))),
            [0x54] => Ok(Some(Op::Load(Reg8::D, Reg8::H))),
            [0x55] => Ok(Some(Op::Load(Reg8::D, Reg8::L))),
            [0x5F] => Ok(Some(Op::Load(Reg8::E, Reg8::A))),
            [0x58] => Ok(Some(Op::Load(Reg8::E, Reg8::B))),
            [0x59] => Ok(Some(Op::Load(Reg8::E, Reg8::C))),
            [0x5A] => Ok(Some(Op::Load(Reg8::E, Reg8::D))),
            [0x5B] => Ok(Some(Op::Load(Reg8::E, Reg8::E))),
            [0x5C] => Ok(Some(Op::Load(Reg8::E, Reg8::H))),
            [0x5D] => Ok(Some(Op::Load(Reg8::E, Reg8::L))),
            [0x67] => Ok(Some(Op::Load(Reg8::H, Reg8::A))),
            [0x60] => Ok(Some(Op::Load(Reg8::H, Reg8::B))),
            [0x61] => Ok(Some(Op::Load(Reg8::H, Reg8::C))),
            [0x62] => Ok(Some(Op::Load(Reg8::H, Reg8::D))),
            [0x63] => Ok(Some(Op::Load(Reg8::H, Reg8::E))),
            [0x64] => Ok(Some(Op::Load(Reg8::H, Reg8::H))),
            [0x65] => Ok(Some(Op::Load(Reg8::H, Reg8::L))),
            [0x6F] => Ok(Some(Op::Load(Reg8::L, Reg8::A))),
            [0x68] => Ok(Some(Op::Load(Reg8::L, Reg8::B))),
            [0x69] => Ok(Some(Op::Load(Reg8::L, Reg8::C))),
            [0x6A] => Ok(Some(Op::Load(Reg8::L, Reg8::D))),
            [0x6B] => Ok(Some(Op::Load(Reg8::L, Reg8::E))),
            [0x6C] => Ok(Some(Op::Load(Reg8::L, Reg8::H))),
            [0x6D] => Ok(Some(Op::Load(Reg8::L, Reg8::L))),
            [0x3E, byte] => Ok(Some(Op::LoadImm(Reg8::A, *byte))),
            [0x06, byte] => Ok(Some(Op::LoadImm(Reg8::B, *byte))),
            [0x0E, byte] => Ok(Some(Op::LoadImm(Reg8::C, *byte))),
            [0x16, byte] => Ok(Some(Op::LoadImm(Reg8::D, *byte))),
            [0x1E, byte] => Ok(Some(Op::LoadImm(Reg8::E, *byte))),
            [0x26, byte] => Ok(Some(Op::LoadImm(Reg8::H, *byte))),
            [0x2E, byte] => Ok(Some(Op::LoadImm(Reg8::L, *byte))),
            [0x0A] => Ok(Some(Op::LoadInd(Reg8::A, Reg16::BC))),
            [0x1A] => Ok(Some(Op::LoadInd(Reg8::A, Reg16::DE))),
            [0x7E] => Ok(Some(Op::LoadInd(Reg8::A, Reg16::HL))),
            [0x46] => Ok(Some(Op::LoadInd(Reg8::B, Reg16::HL))),
            [0x4E] => Ok(Some(Op::LoadInd(Reg8::C, Reg16::HL))),
            [0x56] => Ok(Some(Op::LoadInd(Reg8::D, Reg16::HL))),
            [0x5E] => Ok(Some(Op::LoadInd(Reg8::E, Reg16::HL))),
            [0x66] => Ok(Some(Op::LoadInd(Reg8::H, Reg16::HL))),
            [0x6E] => Ok(Some(Op::LoadInd(Reg8::L, Reg16::HL))),
            [0xFA, h, l] => Ok(Some(Op::LoadIndAImm(util::make_word(*h, *l)))),
            [0x02] => Ok(Some(Op::StoreInd(Reg16::BC, Reg8::A))),
            [0x12] => Ok(Some(Op::StoreInd(Reg16::DE, Reg8::A))),
            [0x77] => Ok(Some(Op::StoreInd(Reg16::HL, Reg8::A))),
            [0x70] => Ok(Some(Op::StoreInd(Reg16::HL, Reg8::B))),
            [0x71] => Ok(Some(Op::StoreInd(Reg16::HL, Reg8::C))),
            [0x72] => Ok(Some(Op::StoreInd(Reg16::HL, Reg8::D))),
            [0x73] => Ok(Some(Op::StoreInd(Reg16::HL, Reg8::E))),
            [0x74] => Ok(Some(Op::StoreInd(Reg16::HL, Reg8::H))),
            [0x75] => Ok(Some(Op::StoreInd(Reg16::HL, Reg8::L))),
            [0x36, byte] => Ok(Some(Op::StoreImmediate(*byte))),
            [0xEA, h, l] => Ok(Some(Op::StoreIndAImmediate(util::make_word(*h, *l)))),
            [0xF0, offset] => Ok(Some(Op::LoadIO(*offset))),
            [0xE0, offset] => Ok(Some(Op::StoreIO(*offset))),
            [0xF2] => Ok(Some(Op::LoadIOC)),
            [0xE2] => Ok(Some(Op::StoreIOC)),
            [0x2A] => Ok(Some(Op::LoadInc)),
            [0x22] => Ok(Some(Op::StoreInc)),
            [0x3A] => Ok(Some(Op::LoadDec)),
            [0x32] => Ok(Some(Op::StoreDec)),
            [0x01, h, l] => Ok(Some(Op::Load16(Reg16::BC, util::make_word(*h, *l)))),
            [0x11, h, l] => Ok(Some(Op::Load16(Reg16::DE, util::make_word(*h, *l)))),
            [0x21, h, l] => Ok(Some(Op::Load16(Reg16::HL, util::make_word(*h, *l)))),
            [0x31, h, l] => Ok(Some(Op::Load16(Reg16::SP, util::make_word(*h, *l)))),
            [0xF9] => Ok(Some(Op::LoadSP)),
            [0xC5] => Ok(Some(Op::Push(Reg16::BC))),
            [0xD5] => Ok(Some(Op::Push(Reg16::DE))),
            [0xE5] => Ok(Some(Op::Push(Reg16::HL))),
            [0xF5] => Ok(Some(Op::Push(Reg16::AF))),
            [0xC1] => Ok(Some(Op::Pop(Reg16::BC))),
            [0xD1] => Ok(Some(Op::Pop(Reg16::DE))),
            [0xE1] => Ok(Some(Op::Pop(Reg16::HL))),
            [0xF1] => Ok(Some(Op::Pop(Reg16::AF))),
            [0x87] => Ok(Some(Op::Add(Reg8::A))),
            [0x80] => Ok(Some(Op::Add(Reg8::B))),
            [0x81] => Ok(Some(Op::Add(Reg8::C))),
            [0x82] => Ok(Some(Op::Add(Reg8::D))),
            [0x83] => Ok(Some(Op::Add(Reg8::E))),
            [0x84] => Ok(Some(Op::Add(Reg8::H))),
            [0x85] => Ok(Some(Op::Add(Reg8::L))),
            [0xC6, byte] => Ok(Some(Op::AddImmediate(*byte))),
            [0x86] => Ok(Some(Op::AddIndHL)),
            [0x8F] => Ok(Some(Op::AddCarry(Reg8::A))),
            [0x88] => Ok(Some(Op::AddCarry(Reg8::B))),
            [0x89] => Ok(Some(Op::AddCarry(Reg8::C))),
            [0x8A] => Ok(Some(Op::AddCarry(Reg8::D))),
            [0x8B] => Ok(Some(Op::AddCarry(Reg8::E))),
            [0x8C] => Ok(Some(Op::AddCarry(Reg8::H))),
            [0x8D] => Ok(Some(Op::AddCarry(Reg8::L))),
            [0xCE, byte] => Ok(Some(Op::AddCarryImmediate(*byte))),
            [0x8E] => Ok(Some(Op::AddCarryIndHL)),
            [0x97] => Ok(Some(Op::Sub(Reg8::A))),
            [0x90] => Ok(Some(Op::Sub(Reg8::B))),
            [0x91] => Ok(Some(Op::Sub(Reg8::C))),
            [0x92] => Ok(Some(Op::Sub(Reg8::D))),
            [0x93] => Ok(Some(Op::Sub(Reg8::E))),
            [0x94] => Ok(Some(Op::Sub(Reg8::H))),
            [0x95] => Ok(Some(Op::Sub(Reg8::L))),
            [0xD6, byte] => Ok(Some(Op::SubImmediate(*byte))),
            [0x96] => Ok(Some(Op::SubIndHL)),
            [0x9F] => Ok(Some(Op::SubCarry(Reg8::A))),
            [0x98] => Ok(Some(Op::SubCarry(Reg8::B))),
            [0x99] => Ok(Some(Op::SubCarry(Reg8::C))),
            [0x9A] => Ok(Some(Op::SubCarry(Reg8::D))),
            [0x9B] => Ok(Some(Op::SubCarry(Reg8::E))),
            [0x9C] => Ok(Some(Op::SubCarry(Reg8::H))),
            [0x9D] => Ok(Some(Op::SubCarry(Reg8::L))),
            [0xDE, byte] => Ok(Some(Op::SubCarryImmediate(*byte))),
            [0x9E] => Ok(Some(Op::SubCarryIndHL)),
            [0xA7] => Ok(Some(Op::And(Reg8::A))),
            [0xA0] => Ok(Some(Op::And(Reg8::B))),
            [0xA1] => Ok(Some(Op::And(Reg8::C))),
            [0xA2] => Ok(Some(Op::And(Reg8::D))),
            [0xA3] => Ok(Some(Op::And(Reg8::E))),
            [0xA4] => Ok(Some(Op::And(Reg8::H))),
            [0xA5] => Ok(Some(Op::And(Reg8::L))),
            [0xE6, byte] => Ok(Some(Op::AndImmediate(*byte))),
            [0xA6] => Ok(Some(Op::AndIndHL)),
            [0xAF] => Ok(Some(Op::Xor(Reg8::A))),
            [0xA8] => Ok(Some(Op::Xor(Reg8::B))),
            [0xA9] => Ok(Some(Op::Xor(Reg8::C))),
            [0xAA] => Ok(Some(Op::Xor(Reg8::D))),
            [0xAB] => Ok(Some(Op::Xor(Reg8::E))),
            [0xAC] => Ok(Some(Op::Xor(Reg8::H))),
            [0xAD] => Ok(Some(Op::Xor(Reg8::L))),
            [0xEE, byte] => Ok(Some(Op::XorImmediate(*byte))),
            [0xAE] => Ok(Some(Op::XorIndHL)),
            [0xB7] => Ok(Some(Op::Or(Reg8::A))),
            [0xB0] => Ok(Some(Op::Or(Reg8::B))),
            [0xB1] => Ok(Some(Op::Or(Reg8::C))),
            [0xB2] => Ok(Some(Op::Or(Reg8::D))),
            [0xB3] => Ok(Some(Op::Or(Reg8::E))),
            [0xB4] => Ok(Some(Op::Or(Reg8::H))),
            [0xB5] => Ok(Some(Op::Or(Reg8::L))),
            [0xF6, byte] => Ok(Some(Op::OrImmediate(*byte))),
            [0xB6] => Ok(Some(Op::OrIndHL)),
            [0xBF] => Ok(Some(Op::Cmp(Reg8::A))),
            [0xB8] => Ok(Some(Op::Cmp(Reg8::B))),
            [0xB9] => Ok(Some(Op::Cmp(Reg8::C))),
            [0xBA] => Ok(Some(Op::Cmp(Reg8::D))),
            [0xBB] => Ok(Some(Op::Cmp(Reg8::E))),
            [0xBC] => Ok(Some(Op::Cmp(Reg8::H))),
            [0xBD] => Ok(Some(Op::Cmp(Reg8::L))),
            [0xFE, byte] => Ok(Some(Op::CmpImmediate(*byte))),
            [0xBE] => Ok(Some(Op::CmpIndHL)),
            [0x3C] => Ok(Some(Op::Inc(Reg8::A))),
            [0x04] => Ok(Some(Op::Inc(Reg8::B))),
            [0x0C] => Ok(Some(Op::Inc(Reg8::C))),
            [0x14] => Ok(Some(Op::Inc(Reg8::D))),
            [0x1C] => Ok(Some(Op::Inc(Reg8::E))),
            [0x24] => Ok(Some(Op::Inc(Reg8::H))),
            [0x2C] => Ok(Some(Op::Inc(Reg8::L))),
            [0x34] => Ok(Some(Op::IncIndHL)),
            [0x3D] => Ok(Some(Op::Dec(Reg8::A))),
            [0x05] => Ok(Some(Op::Dec(Reg8::B))),
            [0x0D] => Ok(Some(Op::Dec(Reg8::C))),
            [0x15] => Ok(Some(Op::Dec(Reg8::D))),
            [0x1D] => Ok(Some(Op::Dec(Reg8::E))),
            [0x25] => Ok(Some(Op::Dec(Reg8::H))),
            [0x2D] => Ok(Some(Op::Dec(Reg8::L))),
            [0x35] => Ok(Some(Op::DecIndHL)),
            [0x27] => Ok(Some(Op::Daa)),
            [0x2F] => Ok(Some(Op::Cpl)),
            [0x09] => Ok(Some(Op::Add16HL(Reg16::BC))),
            [0x19] => Ok(Some(Op::Add16HL(Reg16::DE))),
            [0x29] => Ok(Some(Op::Add16HL(Reg16::HL))),
            [0x39] => Ok(Some(Op::Add16HL(Reg16::SP))),
            [0x03] => Ok(Some(Op::Inc16(Reg16::BC))),
            [0x13] => Ok(Some(Op::Inc16(Reg16::DE))),
            [0x23] => Ok(Some(Op::Inc16(Reg16::HL))),
            [0x33] => Ok(Some(Op::Inc16(Reg16::SP))),
            [0x0B] => Ok(Some(Op::Dec16(Reg16::BC))),
            [0x1B] => Ok(Some(Op::Dec16(Reg16::DE))),
            [0x2B] => Ok(Some(Op::Dec16(Reg16::HL))),
            [0x3B] => Ok(Some(Op::Dec16(Reg16::SP))),
            [0xE8, offset] => Ok(Some(Op::AddSignedSP(*offset))),
            [0xF8, offset] => Ok(Some(Op::LoadSigned(*offset))),
            [0x3F] => Ok(Some(Op::Ccf)),
            [0x37] => Ok(Some(Op::Scf)),
            [0x00] => Ok(Some(Op::Nop)),
            [0x76] => Ok(Some(Op::Halt)),
            [0x10, 0x00] => Ok(Some(Op::Stop)),
            [0xF3] => Ok(Some(Op::Di)),
            [0xFB] => Ok(Some(Op::Ei)),
            [0xC3, h, l] => Ok(Some(Op::Jump(util::make_word(*h, *l)))),
            [0xE9] => Ok(Some(Op::JumpInd)),
            [0x18, offset] => Ok(Some(Op::JumpRel(*offset))),
            [0xCD, h, l] => Ok(Some(Op::Call(util::make_word(*h, *l)))),
            [0xC9] => Ok(Some(Op::Ret)),
            [0x00] => Ok(Some(Op::RetEnable)),
            [0xC7] => Ok(Some(Op::Reset(0x00))),
            [0xCF] => Ok(Some(Op::Reset(0x08))),
            [0xD7] => Ok(Some(Op::Reset(0x10))),
            [0xDF] => Ok(Some(Op::Reset(0x18))),
            [0xE7] => Ok(Some(Op::Reset(0x20))),
            [0xEF] => Ok(Some(Op::Reset(0x28))),
            [0xF7] => Ok(Some(Op::Reset(0x30))),
            [0xFF] => Ok(Some(Op::Reset(0x38))),
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
            Op::LoadIndAImm(src_addr) => {
                let byte = match self.memory.borrow().get(src_addr) {
                    Ok(byte) => byte,
                    Err(_) => return Err(Error::InvalidMemoryAccess),
                };

                self.set_byte_register(Reg8::A, byte);
                Ok(true)
            }
            Op::StoreInd(dest, src) => {
                let dest_addr = self.get_word_register(dest);
                let byte = self.get_byte_register(src);
                match self.memory.borrow_mut().set(dest_addr, byte) {
                    Ok(_) => Ok(true),
                    Err(_) => Err(Error::InvalidMemoryAccess)
                }
            }
            Op::StoreImmediate(byte) => {
                let dest_addr = self.get_word_register(Reg16::HL);
                match self.memory.borrow_mut().set(dest_addr, byte) {
                    Ok(_) => Ok(true),
                    Err(_) => Err(Error::InvalidMemoryAccess)
                }
            }
            Op::StoreIndAImmediate(dest_addr) => {
                let byte = self.get_byte_register(Reg8::A);
                match self.memory.borrow_mut().set(dest_addr, byte) {
                    Ok(_) => Ok(true),
                    Err(_) => Err(Error::InvalidMemoryAccess)
                }
            }
            Op::LoadIO(offset) => {
                let src_addr = 0xFF00 + (offset as u16);

                let byte = match self.memory.borrow().get(src_addr) {
                    Ok(byte) => byte,
                    Err(_) => return Err(Error::InvalidMemoryAccess),
                };

                self.set_byte_register(Reg8::A, byte);
                Ok(true)
            }
            Op::StoreIO(offset) => {
                let dest_addr = 0xFF00 + (offset as u16);
                let byte = self.get_byte_register(Reg8::A);

                match self.memory.borrow_mut().set(dest_addr, byte) {
                    Ok(_) => Ok(true),
                    Err(_) => Err(Error::InvalidMemoryAccess)
                }
            }
            Op::LoadIOC => {
                let src_addr = 0xFF00 + (self.get_byte_register(Reg8::C) as u16);

                let byte = match self.memory.borrow().get(src_addr) {
                    Ok(byte) => byte,
                    Err(_) => return Err(Error::InvalidMemoryAccess),
                };

                self.set_byte_register(Reg8::A, byte);
                Ok(true)
            }
            Op::StoreIOC => {
                let dest_addr = 0xFF00 + (self.get_byte_register(Reg8::C) as u16);
                let byte = self.get_byte_register(Reg8::A);

                match self.memory.borrow_mut().set(dest_addr, byte) {
                    Ok(_) => Ok(true),
                    Err(_) => Err(Error::InvalidMemoryAccess)
                }
            }
            Op::LoadInc => {
                let src_addr = self.get_word_register(Reg16::HL);

                let byte = match self.memory.borrow().get(src_addr) {
                    Ok(byte) => byte,
                    Err(_) => return Err(Error::InvalidMemoryAccess),
                };

                self.set_byte_register(Reg8::A, byte);
                self.set_word_register(Reg16::HL, src_addr + 1);
                Ok(true)
            }
            Op::StoreInc => {
                let dest_addr = self.get_word_register(Reg16::HL);
                let byte = self.get_byte_register(Reg8::A);

                match self.memory.borrow_mut().set(dest_addr, byte) {
                    Ok(_) => {}
                    Err(_) => return Err(Error::InvalidMemoryAccess)
                }

                self.set_word_register(Reg16::HL, dest_addr + 1);
                Ok(true)
            }
            Op::LoadDec => {
                let src_addr = self.get_word_register(Reg16::HL);

                let byte = match self.memory.borrow().get(src_addr) {
                    Ok(byte) => byte,
                    Err(_) => return Err(Error::InvalidMemoryAccess),
                };

                self.set_byte_register(Reg8::A, byte);
                self.set_word_register(Reg16::HL, src_addr - 1);
                Ok(true)
            }
            Op::StoreDec => {
                let dest_addr = self.get_word_register(Reg16::HL);
                let byte = self.get_byte_register(Reg8::A);

                match self.memory.borrow_mut().set(dest_addr, byte) {
                    Ok(_) => {}
                    Err(_) => return Err(Error::InvalidMemoryAccess)
                }

                self.set_word_register(Reg16::HL, dest_addr - 1);
                Ok(true)
            }
            Op::Load16(dest, word) => {
                self.set_word_register(dest, word);
                Ok(true)
            }
            Op::LoadSP => {
                self.set_word_register(Reg16::SP, self.get_word_register(Reg16::HL));
                Ok(true)
            }
            Op::Push(reg) => {
                unimplemented!("SP=SP-2  (SP)=rr");
                Ok(true)
            }
            Op::Pop(reg) => {
                unimplemented!("rr=(SP)  SP=SP+2");
                Ok(true)
            }

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
            Reg8::A => util::get_high_byte(self.registers[0]),
            Reg8::B => util::get_high_byte(self.registers[1]),
            Reg8::C => util::get_low_byte(self.registers[1]),
            Reg8::D => util::get_high_byte(self.registers[2]),
            Reg8::E => util::get_low_byte(self.registers[2]),
            Reg8::H => util::get_high_byte(self.registers[3]),
            Reg8::L => util::get_low_byte(self.registers[3]),
        }
    }

    fn set_byte_register(&mut self, reg: Reg8, byte: u8) {
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

    fn get_word_register(&self, reg: Reg16) -> u16 {
        match reg {
            Reg16::AF => self.registers[0],
            Reg16::BC => self.registers[1],
            Reg16::DE => self.registers[2],
            Reg16::HL => self.registers[3],
            Reg16::SP => self.sp,
            Reg16::PC => self.pc,
        }
    }

    fn set_word_register(&mut self, reg: Reg16, word: u16) {
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
}
