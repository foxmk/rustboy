// util.rs

pub fn get_bit(byte: u8, n: u8) -> bool {
    byte & (1 << n) == (1 << n)
}

pub fn set_bit(byte: &mut u8, n: u8, val: bool) {
    let shift = if val { 0x1 } else { 0x0 };
    let mask = (1 as u8) << n;
    *byte = (!mask & *byte) | shift
}

pub fn get_high_byte(word: u16) -> u8 {
    (word >> 8) as u8
}

pub fn get_low_byte(word: u16) -> u8 {
    (word & 0x00FF) as u8
}

pub fn make_word(h: u8, l: u8) -> u16 {
    let h: u16 = h as u16;
    let l: u16 = l as u16;
    (h << 8) + l
}

pub fn set_high_byte(word: &mut u16, byte: u8) {
    let shift = (byte as u16) << 8;
    let mask = (0xFF as u16) << 8;
    *word = (!mask & *word) | shift;
}

pub fn set_low_byte(word: &mut u16, byte: u8) {
    let shift = byte as u16;
    let mask = 0xFF as u16;
    *word = (!mask & *word) | shift;
}