use std::{fs::{File, self}, io::{BufReader, Read, Write, stdout}};

use bus::BasicCPUMemory;
use cpu::Cpu;
use crossterm::{ExecutableCommand, style::Print, terminal::Clear, execute, event::{read, Event, KeyCode, KeyEventKind}};

use crate::bus::CPUMemory;

pub mod bus;
pub mod cpu;
mod cpu_tests;

fn main() {
    let nestest = fs::read("test_rom/nestest.nes").unwrap();

    let mut memory_vec = vec![0; 65536];
    let start = 0xC000;
    let length = 16384;

    for i in 0..length {
        memory_vec[start + i] = nestest[0x10 + i];
    }

    let nestest_mem = BasicCPUMemory::try_from(memory_vec).unwrap();
    let mut cpu = Cpu::new(nestest_mem);

    cpu.program_counter = 0xC000;
    loop {
        execute!(
            stdout(),
            Clear(crossterm::terminal::ClearType::All),
            Print(&cpu),
            Print(format!("{:#X} {:#X}\n", cpu.bus.read(0x2), cpu.bus.read(0x3))),
            Print("Z - 1 steps X - 100 steps C - 1K steps V - 1M steps\n")
        );

        match read().unwrap() {
            Event::Key(event) => {
                if event.kind == KeyEventKind::Press {
                    match event.code {
                        KeyCode::Char('z') => {
                            cpu.step();
                        },
                        KeyCode::Char('x') => {
                            for _ in 0..100 {
                                cpu.step();
                            }
                        },
                        KeyCode::Char('c') => {
                            for _ in 0..1000 {
                                cpu.step();
                            }
                        },
                        KeyCode::Char('v') => {
                            for _ in 0..1000000 {
                                cpu.step();
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
}
