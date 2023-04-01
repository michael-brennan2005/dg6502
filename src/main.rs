use std::{fs::{self}, io::{stdout}};

use crossterm::{style::Print, terminal::Clear, execute, event::{read, Event, KeyCode, KeyEventKind}};
use dg6502::{Cpu, CpuConfig, BasicCPUMemory, StatusRegister, CPUMemory};

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
    let mut status = StatusRegister::default();
    status.ignored = true;
    status.interrupt = true;
    let mut cpu = Cpu::new(nestest_mem, CpuConfig::default(), status);

    cpu.program_counter = 0xC000;
    let mut steps: usize = 0;

    loop {
        execute!(
            stdout(),
            Clear(crossterm::terminal::ClearType::All),
            Print(&cpu),
            Print(format!("{:#X} {:#X} | {}\n", cpu.bus.read(0x2), cpu.bus.read(0x3), steps)),
            Print("Z - 1 steps X - 100 steps C - 1K steps V - 1M steps\n")
        );

        match read().unwrap() {
            Event::Key(event) => {
                if event.kind == KeyEventKind::Press {
                    match event.code {
                        KeyCode::Char('z') => {
                            cpu.step();
                            steps += 1;
                        },
                        KeyCode::Char('x') => {
                            for _ in 0..100 {
                                cpu.step();
                                steps += 1;
                            }
                        },
                        KeyCode::Char('c') => {
                            for _ in 0..1000 {
                                cpu.step();
                                steps += 1;
                            }
                        },
                        KeyCode::Char('v') => {
                            for _ in 0..1000000 {
                                cpu.step();
                                steps += 1;
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
