// main.rs
#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]

use minifb::{Key, Window, WindowOptions};

mod cpu;
mod memory;
mod util;
mod video;

const WIDTH: usize = 160;
const HEIGHT: usize = 144;

fn main() {
    println!("Hello");

    //    let mut buffer: Vec<u32> = vec![0; WIDTH * HEIGHT];
    //
    //    let window_options = WindowOptions::default();
    //    let mut window = Window::new("Test - ESC to exit", WIDTH, HEIGHT, window_options).unwrap_or_else(|e| {
    //        panic!("{}", e);
    //    });
    //
    //    while window.is_open() && !window.is_key_down(Key::Escape) {
    //        for i in buffer.iter_mut() {
    //            *i = 0; // write something more funny here!
    //        }
    //
    //        window.update_with_buffer(&buffer).unwrap();
    //    }
}
