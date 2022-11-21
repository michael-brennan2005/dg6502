use std::{fs::File, io::{BufReader, Read}};

use crate::bus::Bus;

pub mod bus;
pub mod cpu;
mod cpu_tests;

fn main() {
    let file = File::open("nestest.nes").unwrap();
    let mut reader = BufReader::new(file);
    let mut buffer: Vec<u8> = Vec::new();
    reader.read_to_end(&mut buffer).unwrap();
    let bus = bus::NESTestBus::try_from(buffer).unwrap();

    let mut cpu6502 = cpu::Cpu::new(bus);
    cpu6502.program_counter = 0xC000;

    loop {
        cpu6502.step();
        println!("PC: {:#06X} X: {:#04X} Y: {:#04X} ACC: {:#04X}, SP: {:#04X}, STATUS: {:#010b}, 0x02: {:#04X}, 0x03: {:#04X}",
            cpu6502.program_counter,
            cpu6502.x,
            cpu6502.y,
            cpu6502.accumulator,
            cpu6502.stack_pointer,
            cpu6502.status.to_u8(),
            cpu6502.bus.read(0x2),
            cpu6502.bus.read(0x3));
    }
}
