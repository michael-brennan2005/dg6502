use std::{fs::File, io::{BufReader, Read, Write, stdout}};

use bus::BasicCPUMemory;
use cpu::Cpu;
use crossterm::{ExecutableCommand, style::Print, terminal::Clear, execute, event::{read, Event, KeyCode, KeyEventKind}};

pub mod bus;
pub mod cpu;
mod cpu_tests;

fn main() {
    let memory = BasicCPUMemory::from_file(&"test_exe/6502_functional_test.bin".to_string()).unwrap();
    let mut cpu = Cpu::new(memory);

    cpu.program_counter = 0x400;
    loop {
        execute!(
            stdout(),
            Clear(crossterm::terminal::ClearType::All),
            Print(&cpu),
            Print("Z - 1 steps X - 100 steps C - 1K steps V - 1M steps\n")
        );

        match read().unwrap() {
            Event::Key(event) => {
                if event.kind == KeyEventKind::Press {
                    match event.code {
                        KeyCode::Char('z') => {
                            cpu.step();
                        },
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
}
