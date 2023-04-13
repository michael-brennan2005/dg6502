use std::{fs::{self}, io::{stdout}, process::exit};

use clap::{Command, command, arg, Parser, Subcommand};
use clap_num::maybe_hex;
use crossterm::{style::Print, terminal::Clear, execute, event::{read, Event, KeyCode, KeyEventKind}};
use dg6502::{Cpu, CpuConfig, BasicCPUMemory, StatusRegister, CPUMemory, JamBehavior, IllegalBehavior, CpuStepReturn};

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

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Debugger {
    #[command(subcommand)]
    command: Commands
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Step through the program.
    Step {
        /// How may CPU steps to take.
        #[clap(value_parser=maybe_hex::<usize>)]
        _steps: Option<usize>
    },
    /// Print out memory.
    GetMemory {
        /// Starting byte you'd like printed.
        #[clap(value_parser=maybe_hex::<u16>)]
        start: u16,
        /// Ending byte you'd like printed.
        #[clap(value_parser=maybe_hex::<u16>)]
        end: Option<u16>,
    },
    /// Set register to a desired value. 
    SetRegister {
        #[command(subcommand)]
        registers: Registers
    },
    /// Set memory to a desired value.
    SetMemory {
        /// Value you'd like to set memory to.
        #[clap(value_parser=maybe_hex::<u8>)]
        value: u8,
        /// Starting byte you'd like printed.
        #[clap(value_parser=maybe_hex::<u16>)]
        start: u16,
        /// Ending byte you'd like printed.
        #[clap(value_parser=maybe_hex::<u16>)]
        end: Option<u16>
    }
}

#[derive(Subcommand, Debug)]
enum Registers {
    PC {
        #[clap(value_parser=maybe_hex::<u16>)]
        x: u16
    },
    ACC {
        #[clap(value_parser=maybe_hex::<u8>)]
        x: u8
    },
    X {
        #[clap(value_parser=maybe_hex::<u8>)]
        x: u8
    },
    Y {
        #[clap(value_parser=maybe_hex::<u8>)]
        x: u8
    },
    STAT {
        #[clap(value_parser=maybe_hex::<u8>)]
        x: u8
    },
    SP {
        #[clap(value_parser=maybe_hex::<u8>)]
        x: u8
    }
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

    cpu.program_counter = 0x0000;
    let mut steps: usize = 0;
    let mut cycles: usize = 0;
    let mut jammed: bool = false;
    loop {
        println!("{}", &cpu);
        println!("Steps: {} | Cycles: {} | Jammed: {}", steps, cycles, cpu.jammed);

        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();

        line = line.trim().to_string();

        let mut start: Vec<&str> = vec![""];
        let mut args: Vec<&str> = line.split(' ').collect();
        start.append(&mut args);
        let command = match Debugger::try_parse_from(start) {
            Ok(cmd) => cmd,
            Err(err) => {
                err.print().unwrap();
                continue;
            },
        };
        
        match command.command {
            Commands::Step { _steps } => {
                let to_take = match _steps {
                    Some(x) => x,
                    None => 1,
                };

                for _ in 0..to_take {
                    let result = cpu.step();
                    match result {
                        CpuStepReturn::Ok(x) => {
                            cycles += x;
                        },
                        CpuStepReturn::Jam => {
                            continue;
                        }
                    }
                    steps += 1;
                }
            },
            Commands::GetMemory { start, end } => {
                match end {
                    Some(end) => {
                        let starting_page = start & 0xFFF0;
                        let ending_page = (end & 0xFFF0) + 0x10;
                        let page_numbers = (ending_page - starting_page + 16 - 1) / 16;

                        println!("     | 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F");
                        for i in 0..page_numbers {
                            println!("{:04X} | {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X}", 
                                (starting_page + i * 0x10),
                                cpu.bus.read(starting_page + i * 0x10),
                                cpu.bus.read(starting_page + 1 + i * 0x10),
                                cpu.bus.read(starting_page + 2 + i * 0x10),
                                cpu.bus.read(starting_page + 3 + i * 0x10),
                                cpu.bus.read(starting_page + 4 + i * 0x10),
                                cpu.bus.read(starting_page + 5 + i * 0x10),
                                cpu.bus.read(starting_page + 6 + i * 0x10),
                                cpu.bus.read(starting_page + 7 + i * 0x10),
                                cpu.bus.read(starting_page + 8 + i * 0x10),
                                cpu.bus.read(starting_page + 9 + i * 0x10),
                                cpu.bus.read(starting_page + 10 + i * 0x10),
                                cpu.bus.read(starting_page + 11 + i * 0x10),
                                cpu.bus.read(starting_page + 12 + i * 0x10),
                                cpu.bus.read(starting_page + 13 + i * 0x10),
                                cpu.bus.read(starting_page + 14 + i * 0x10),
                                cpu.bus.read(starting_page + 15 + i * 0x10)
                            )
                        }
                    },
                    None => {
                        println!("{:#4X}", cpu.bus.read(start))
                    },
                }
            },
            Commands::SetRegister { registers } => {
                match registers {
                    Registers::PC { x } => cpu.program_counter = x,
                    Registers::ACC { x } => cpu.accumulator = x,
                    Registers::X { x } => cpu.x = x,
                    Registers::Y { x } => cpu.y = x,
                    Registers::STAT { x } => cpu.status = StatusRegister::from_u8(x),
                    Registers::SP { x } => cpu.stack_pointer = x,
                }
            },
            Commands::SetMemory { value, start, end } => {
                match end {
                    Some(x) => {
                        for i in start..x {
                            cpu.bus.write(i, value);
                        }
                    },
                    None => {
                        cpu.bus.write(start, value);
                    },
                }
            },
        }

    }
}
