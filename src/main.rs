use std::{fs::File, io::BufReader};
use std::io::Read;

pub mod bus;
pub mod cpu;

fn main() {
    let f = File::open("example.bin").unwrap();
    let mut reader = BufReader::new(f);
    let mut buffer: Vec<u8> = Vec::new();

    reader.read_to_end(&mut buffer).unwrap();

    let bus = bus::BasicBus::try_from(buffer).unwrap();
    let mut cpu6502 = cpu::Cpu::new(bus);

    cpu6502.reset();
    while (cpu6502.program_counter < 16) {
        cpu6502.step();
    }
}
