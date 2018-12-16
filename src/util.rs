// util.rs

pub fn bit_enabled(byte: u8, bit: u8) -> bool {
    byte & (1 << bit) == (1 << bit)
}
