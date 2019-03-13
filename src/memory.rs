// memory.rs

use std;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::ops::RangeInclusive;
use std::rc::Weak;
use std::rc::Rc;

pub mod constants {
    const ROM_FIXED_START: u16 = 0x0000;
    const ROM_FIXED_END: u16 = 0x3FFF;
    const ROM_SWITCHABLE_START: u16 = 0x4000;
    const ROM_SWITCHABLE_END: u16 = 0x7FFF;
    const EXTERNAL_RAM_START: u16 = 0xA000;
    const EXTERNAL_RAM_END: u16 = 0xBFFF;
    const RAM_FIXED_START: u16 = 0xC000;
    const RAM_FIXED_END: u16 = 0xCFFF;
    const RAM_SWITCHABLE_START: u16 = 0xD000;
    const RAM_SWITCHABLE_END: u16 = 0xDFFF;
    const ECHO_RAM_START: u16 = 0xE000;
    const ECHO_RAM_END: u16 = 0xFDFF;
    const UNUSED_START: u16 = 0xFEA0;
    const UNUSED_END: u16 = 0xFEFF;
    const IO_REGISTERS_START: u16 = 0xFF00;
    const IO_REGISTERS_END: u16 = 0xFF7F;
    const HRAM_START: u16 = 0xFF80;
    const HRAM_END: u16 = 0xFFFE;
    const IE_START: u16 = 0xFFFF;
    const IE_END: u16 = 0xFFFF;
}

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    Unavailable(u16),
    OutOfBounds,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Error::Unavailable(addr) => write!(f, "Memory address {:06x} unavailable", addr),
            Error::OutOfBounds => write!(f, "Some error"),
        }
    }
}

pub trait Addressable {
    fn address_ranges(&self) -> Vec<RangeInclusive<u16>>;
    fn read(&self, address: u16) -> Result<u8, Error>;
    fn write(&self, address: u16, byte: u8) -> Result<(), Error>;
}

pub struct AddressSpace {
    map: HashMap<RangeInclusive<u16>, Weak<dyn Addressable>>,
}

impl AddressSpace {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn register_space(&mut self, storage: Rc<dyn Addressable>) {
        for range in &storage.address_ranges() {
            self.map.insert(range.clone(), Rc::downgrade(&storage));
        }
    }
}

impl Addressable for AddressSpace {
    fn address_ranges(&self) -> Vec<RangeInclusive<u16>> {
        vec![0x0000..=std::u16::MAX]
    }

    fn read(&self, addr: u16) -> Result<u8, Error> {
        for (range, storage) in &self.map {
            if &addr >= range.start() && &addr <= range.end() {
                match storage.upgrade() {
                    None => return Err(Error::Unavailable(addr)),
                    Some(storage) => return storage.read(addr),
                }
            }
        }

        return Err(Error::Unavailable(addr));
    }

    fn write(&self, addr: u16, byte: u8) -> Result<(), Error> {
        for (range, storage) in &self.map {
            if &addr >= range.start() && &addr <= range.end() {
                match storage.upgrade() {
                    None => return Err(Error::Unavailable(addr)),
                    Some(storage) => return storage.write(addr, byte),
                }
            }
        }

        return Err(Error::Unavailable(addr));
    }
}

#[cfg(test)]
pub mod test_util {
    use std;
    use std::cell::{RefCell, Ref};
    use std::ops::RangeInclusive;
    use std::rc::Rc;

    use crate::memory;
    use crate::memory::Addressable;

    pub struct VecMemory {
        bytes: RefCell<Vec<u8>>,
        range: RangeInclusive<u16>,
        base_offset: u16,
    }

    impl VecMemory {
        pub fn new(bytes: Vec<u8>) -> Self {
            Self::new_offset(bytes, 0x0000)
        }

        pub fn new_offset(bytes: Vec<u8>, base_offset: u16) -> Self {
            let max_addr = base_offset + bytes.len() as u16 - 1;
            let range = base_offset..=max_addr;

            VecMemory { bytes: RefCell::new(bytes), range, base_offset }
        }
    }

    impl memory::Addressable for VecMemory {
        fn address_ranges(&self) -> Vec<RangeInclusive<u16>> {
            vec![self.range.clone()]
        }

        fn read(&self, addr: u16) -> Result<u8, memory::Error> {
            if &addr >= self.range.start() && &addr <= self.range.end() {
                let i = addr - self.base_offset;
                Ok(self.bytes.borrow()[i as usize])
            } else {
                Err(memory::Error::OutOfBounds)
            }
        }

        fn write(&self, addr: u16, byte: u8) -> Result<(), memory::Error> {
            if &addr >= self.range.start() && &addr <= self.range.end() {
                let i = addr - self.base_offset;
                self.bytes.borrow_mut()[i as usize] = byte;
                Ok(())
            } else {
                Err(memory::Error::OutOfBounds)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;

    use super::test_util::*;
    use super::*;
    use std::ops::{Range, Add};

    #[test]
    fn empty_space() {
        let space = AddressSpace::new();
        assert_eq!(space.read(0x0000), Err(Error::Unavailable(0x0000)))
    }

    #[test]
    fn register_space() {
        let mut space = AddressSpace::new();

        let memory = Rc::new(VecMemory::new(vec![0xFF]));
        space.register_space(memory.clone());

        assert_eq!(space.read(0x0000), memory.read(0x0000));
    }

    #[test]
    fn out_of_bounds() {
        let mut space = AddressSpace::new();

        let memory = Rc::new(VecMemory::new(vec![0xFF]));
        space.register_space(memory.clone());

        assert_eq!(space.read(0x0001), Err(Error::Unavailable(0x0001)));
    }

    #[test]
    fn write_byte() {
        let mut space = AddressSpace::new();

        let memory = Rc::new(VecMemory::new(vec![0x00; 0x00FF - 1]));
        space.register_space(memory.clone());

        space.write(0x0020, 0xFF);
        assert_eq!(memory.read(0x0020), Ok(0xFF));
        assert_eq!(space.read(0x0020), Ok(0xFF));
    }

    #[test]
    fn multiple_storage() {
        let mut space = AddressSpace::new();

        let memory_one = Rc::new(VecMemory::new_offset(vec![0x00; 0x00FF + 1], 0x0000));
        space.register_space(memory_one.clone());

        let memory_two = Rc::new(VecMemory::new_offset(vec![0x00; 0x00FF + 1], 0x0100));
        space.register_space(memory_two.clone());

        space.write(0x0020, 0xFF);
        assert_eq!(memory_one.read(0x0020), Ok(0xFF));
        assert_eq!(space.read(0x0020), Ok(0xFF));

        space.write(0x0120, 0xFE);
        assert_eq!(memory_two.read(0x0120), Ok(0xFE));
        assert_eq!(space.read(0x0120), Ok(0xFE));
    }
}
