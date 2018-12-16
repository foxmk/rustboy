// memory.rs

use std;
use std::cell::RefCell;
use std::fmt;
use std::ops::RangeInclusive;
use std::rc::Weak;
use std::collections::HashMap;

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
    fn get(&self, address: u16) -> Result<u8, Error>;
    fn set(&mut self, address: u16, byte: u8) -> Result<(), Error>;
}

pub struct AddressSpace {
    map: HashMap<RangeInclusive<u16>, Weak<RefCell<dyn Addressable>>>
}

impl AddressSpace {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn register_space(&mut self, storage: Weak<RefCell<dyn Addressable>>) -> Result<(), Error> {
        match storage.upgrade() {
            Some(s) => {
                s.borrow().address_ranges().into_iter().for_each(|range| {
                    self.map.insert(range, storage.clone());
                });
                Ok(())
            }
            None => Err(Error::OutOfBounds)
        }
    }
}

impl Addressable for AddressSpace {
    fn address_ranges(&self) -> Vec<RangeInclusive<u16>> {
        vec![0x0000..=std::u16::MAX]
    }

    fn get(&self, addr: u16) -> Result<u8, Error> {
        for (range, storage) in &self.map {
            if &addr >= range.start() && &addr <= range.end() {
                match storage.upgrade() {
                    None => return Err(Error::Unavailable(addr)),
                    Some(storage) => return storage.borrow().get(addr),
                }
            }
        };

        return Err(Error::Unavailable(addr));
    }

    fn set(&mut self, addr: u16, byte: u8) -> Result<(), Error> {
        for (range, storage) in &self.map {
            if &addr >= range.start() && &addr <= range.end() {
                match storage.upgrade() {
                    None => return Err(Error::Unavailable(addr)),
                    Some(storage) => return storage.borrow_mut().set(addr, byte),
                }
            }
        };

        return Err(Error::Unavailable(addr));
    }
}

#[cfg(test)]
pub mod test_util {
    use std;
    use std::cell::RefCell;
    use std::ops::RangeInclusive;
    use std::rc::Rc;

    use crate::memory;
    use crate::memory::Addressable;

    pub struct VecMemory {
        vec: Vec<u8>,
        range: RangeInclusive<u16>,
    }

    impl VecMemory {
        pub fn new_ref(bytes: Vec<u8>) -> Rc<RefCell<Self>> {
            let end_addr = (bytes.len() as u16) - 1;
            VecMemory::new_ref_ranged(bytes, 0x0000..=end_addr)
        }

        pub fn new_ref_ranged(vec: Vec<u8>, range: RangeInclusive<u16>) -> Rc<RefCell<Self>> {
            Rc::new(RefCell::new(VecMemory {
                vec,
                range,
            }))
        }
    }

    impl memory::Addressable for VecMemory {
        fn address_ranges(&self) -> Vec<RangeInclusive<u16>> {
            vec![self.range.clone()]
        }

        fn get(&self, addr: u16) -> Result<u8, memory::Error> {
            if &addr >= self.range.start() && &addr <= self.range.end() {
                match self.vec.get(addr as usize) {
                    None => Ok(0x00),
                    Some(i) => Ok(*i)
                }
            } else {
                Err(memory::Error::OutOfBounds)
            }
        }

        fn set(&mut self, addr: u16, byte: u8) -> Result<(), memory::Error> {
            if &addr >= self.range.start() && &addr <= self.range.end() {
                match self.vec.get_mut(addr as usize) {
                    Some(i) => *i = byte,
                    None => {
                        self.vec.resize(addr as usize + 1, 0x00);
                        match self.vec.get_mut(addr as usize) {
                            None => unreachable!(),
                            Some(i) => *i = byte
                        }
                    }
                }

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

    use super::*;
    use super::test_util::*;

    #[test]
    fn empty_space() {
        let space = AddressSpace::new();
        assert_eq!(space.get(0x0000), Err(Error::Unavailable(0x0000)))
    }

    #[test]
    fn register_space() {
        let mut space = AddressSpace::new();

        let memory: Rc<RefCell<Addressable>> = VecMemory::new_ref(vec![0xFF]);
        space.register_space(Rc::downgrade(&memory));

        assert_eq!(space.get(0x0000), memory.borrow().get(0x0000));
    }

    #[test]
    fn out_of_bounds() {
        let mut space = AddressSpace::new();

        let memory: Rc<RefCell<Addressable>> = VecMemory::new_ref(vec![0xFF]);
        space.register_space(Rc::downgrade(&memory));

        assert_eq!(space.get(0x0001), Err(Error::Unavailable(0x0001)));
    }

    #[test]
    fn write_byte() {
        let mut space = AddressSpace::new();

        let memory: Rc<RefCell<Addressable>> = VecMemory::new_ref_ranged(vec![], 0x0000..=0x00FF);
        space.register_space(Rc::downgrade(&memory));

        space.set(0x0020, 0xFF);
        assert_eq!(memory.borrow().get(0x0020), Ok(0xFF));
        assert_eq!(space.get(0x0020), Ok(0xFF));
    }

    #[test]
    fn multiple_storage() {
        let mut space = AddressSpace::new();

        let memory_one: Rc<RefCell<Addressable>> = VecMemory::new_ref_ranged(vec![], 0x0000..=0x00FF);
        space.register_space(Rc::downgrade(&memory_one));

        let memory_two: Rc<RefCell<Addressable>> = VecMemory::new_ref_ranged(vec![], 0x0100..=0x01FF);
        space.register_space(Rc::downgrade(&memory_two));

        space.set(0x0020, 0xFF);
        assert_eq!(memory_one.borrow().get(0x0020), Ok(0xFF));
        assert_eq!(space.get(0x0020), Ok(0xFF));

        space.set(0x0120, 0xFE);
        assert_eq!(memory_two.borrow().get(0x0120), Ok(0xFE));
        assert_eq!(space.get(0x0120), Ok(0xFE));
    }
}