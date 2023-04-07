use std::{fs::{self}, io::{stdout}, process::exit};

use clap::{Command, command, arg, Parser};
use clap_num::maybe_hex;
use crossterm::{style::Print, terminal::Clear, execute, event::{read, Event, KeyCode, KeyEventKind}};
use dg6502::{Cpu, CpuConfig, BasicCPUMemory, StatusRegister, CPUMemory, JamBehavior, IllegalBehavior};

mod cpu_tests;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// File to load.
    name: String,

    /// From what byte of the file to begin loading from.
    #[arg(short='r', long, value_parser=maybe_hex::<u16>)]
    byte_offset_rom: Option<u16>,

    /// From what byte of memory to begin loading to.
    #[arg(short='m', long, value_parser=maybe_hex::<u16>)]
    byte_offset_mem: Option<u16>,

    /// How many bytes to load.
    #[arg(short, long, value_parser=maybe_hex::<u16>)]
    byte_count: Option<u16>,

    #[arg(short, long, value_enum)]
    jam: Option<JamBehavior>,

    #[arg(short, long, value_enum)]
    illegal: Option<IllegalBehavior>,

    // Allow BCD support or not.
    #[arg(short='d', long)]
    bcd_support: Option<bool>

}
fn main() {
    let cli = Cli::parse();
    let rom = fs::read(cli.name).unwrap();
    
    let mut memory_vec = vec![0; 65536];
    
    let start_rom: usize = match cli.byte_offset_rom {
        Some(x) => usize::from(x),
        None => 0x0,
    };

    let start_mem: usize = match cli.byte_offset_mem {
        Some(x) => usize::from(x),
        None => 0x0,
    };

    let count: usize = match cli.byte_count {
        Some(x) => usize::from(x),
        None => 65536 - start_mem,
    };

    for i in 0..count {
        memory_vec[start_mem + i] = rom[start_rom + i];
    }

    let nestest_mem = BasicCPUMemory::try_from(memory_vec).unwrap();

    let bcd_support = match cli.bcd_support {
        Some(x) => x,
        None => true,
    };

    let jam_behavior = match cli.jam {
        Some(x) => x,
        None => JamBehavior::Jam,
    };

    let illegal_behavior = match cli.illegal {
        Some(x) => x,
        None => IllegalBehavior::Execute,
    };

    let mut cpu = Cpu::new(nestest_mem, CpuConfig { bcd_support, jam_behavior, illegal_behavior }, StatusRegister::default());

    // MORNING!: add a parser for lldb-type instructions
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
