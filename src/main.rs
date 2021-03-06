// main.rs
#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]

use std::cell::RefCell;
use std::rc::Rc;

use minifb::{Key, Window, WindowOptions};

use crate::cpu::CPU;
use crate::interrupts::InterruptController;
use crate::memory::AddressSpace;
use crate::ppu::PPU;

mod cpu;
mod memory;
mod util;
mod ppu;
mod ops;
mod interrupts;

const WIDTH: usize = 160;
const HEIGHT: usize = 144;

fn main() {
    let mut buffer: Vec<u32> = vec![0; WIDTH * HEIGHT];

    let window_options = WindowOptions::default();
    let mut window = Window::new("Test - ESC to exit", WIDTH, HEIGHT, window_options).unwrap_or_else(|e| {
        panic!("{}", e);
    });


    let address_space = Rc::new(RefCell::new(AddressSpace::new()));

    let ppu = Rc::new(RefCell::new(PPU::new(address_space.clone())));
    address_space.borrow_mut().register_space(ppu.clone());

    let int_c = Rc::new(RefCell::new(InterruptController::new()));
    address_space.borrow_mut().register_space(int_c.clone());

    let cpu = Rc::new(RefCell::new(CPU::new(address_space.clone())));

    cpu.borrow_mut().step();
    ppu.borrow_mut().step();

    while window.is_open() && !window.is_key_down(Key::Escape) {
        for i in buffer.iter_mut() {
            *i = 0; // write something more funny here!
        }

        window.update_with_buffer(&buffer).unwrap();
    }
}
