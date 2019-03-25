use std;
use std::fmt;
use std::ops::{Index, IndexMut};

pub trait Byte {
    fn get_hi(&self) -> u8;
    fn get_low(&self) -> u8;

    fn set_hi(&mut self, byte: u8);
    fn set_low(&mut self, byte: u8);
}

impl Byte for u16 {
    fn get_hi(&self) -> u8 {
        (*self >> 8) as u8
    }

    fn get_low(&self) -> u8 {
        (*self & 0x00FF) as u8
    }

    fn set_hi(&mut self, byte: u8) {
        let shift = (byte as u16) << 8;
        let mask = (0xFF as u16) << 8;
        *self = (!mask & *self) | shift;
    }

    fn set_low(&mut self, byte: u8) {
        let shift = byte as u16;
        let mask = 0xFF as u16;
        *self = (!mask & *self) | shift;
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    InvalidBitValue(usize),
    InvalidBitPosition(usize),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Error::InvalidBitPosition(n) => write!(f, "{}", n),
            Error::InvalidBitValue(n) => write!(f, "{}", n)
        }
    }
}

pub trait Bit {
    fn get_bit(&self, n: usize) -> Result<usize, Error>;
    fn set_bit(&mut self, n: usize, bit: usize) -> Result<(), Error>;
}

impl Bit for u16 {
    fn get_bit(&self, n: usize) -> Result<usize, Error> {
        if n > 15 {
            Err(Error::InvalidBitPosition(n))
        } else {
            Ok(((*self) & (1 << n) == (1 << n)) as usize)
        }
    }

    fn set_bit(&mut self, n: usize, bit: usize) -> Result<(), Error> {
        if n > 15 {
            Err(Error::InvalidBitPosition(n))
        } else if bit > 1 {
            Err(Error::InvalidBitValue(n))
        } else {
            let mask = (1 as u16) << n;
            Ok(*self = (!mask & *self) | (bit as u16))
        }
    }
}

impl Bit for u8 {
    fn get_bit(&self, n: usize) -> Result<usize, Error> {
        if n > 7 {
            Err(Error::InvalidBitPosition(n))
        } else {
            Ok(((*self) & (1 << n) == (1 << n)) as usize)
        }
    }

    fn set_bit(&mut self, n: usize, bit: usize) -> Result<(), Error> {
        if n > 7 {
            Err(Error::InvalidBitPosition(n))
        } else if bit > 1 {
            Err(Error::InvalidBitValue(n))
        } else {
            let mask = (1 as u8) << n;
            Ok(*self = (!mask & *self) | (bit as u8))
        }
    }
}

pub fn make_word(h: u8, l: u8) -> u16 {
    let h: u16 = h as u16;
    let l: u16 = l as u16;
    (h << 8) + l
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn should_work() {
        let r: u16 = 0xdead;

        assert_eq!(0xde, r.get_hi());
        assert_eq!(0xad, r.get_low());
    }

    #[test]
    fn should_work_2() {
        let r: u16 = 0b0110_1001;

        assert_eq!(Ok(1), r.get_bit(0));
        assert_eq!(Ok(0), r.get_bit(1));
        assert_eq!(Ok(0), r.get_bit(2));
        assert_eq!(Ok(1), r.get_bit(3));
        assert_eq!(Ok(0), r.get_bit(4));
        assert_eq!(Ok(1), r.get_bit(5));
        assert_eq!(Ok(1), r.get_bit(6));
        assert_eq!(Ok(0), r.get_bit(7));
    }
}