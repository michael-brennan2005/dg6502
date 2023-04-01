//! A crate for emulating the MOS6502 processor. Meant to be used for 
//! emulators whose systems used this chip or ones similar to it, such as the NES or Commodore 64.
//! 
//! Using dg6502 first requires constructing a CPUMemory object that will be used by the CPU to read and
//! write to memory, then constructing a CPU object. Instructions can be executed calling the CPU's step() function.
//! 
//! Support for using this as an executable is not yet done: the current main.rs code just runs nestest.
//! ## Basic Example
//! ```
//! use dg6502::{Cpu, BasicCPUMemory, CpuConfig, JamBehavior, IllegalBehavior}
//! 
//! // Load a program from a file into memory, starting at 0x0.
//! let memory = BasicCPUMemory::from_file(&String::from("my_file.bin"), 0x0)?;
//! 
//! // Configure our CPU and our starting status register.
//! let config = CpuConfig::default()
//!     .bcd_support(true) // allow BCD arithmetic
//!     .jam_behavior(JamBehavior::Nop) // treat JAMs as NOPs
//!     .illegal_behavior(IllegalBehavior::Execute); // run illegal instructions
//! 
//! let status = StatusRegister::default();
//! 
//! // Initialize CPU and set our program counter to 0x0 
//! // (set to 0x0 by default, this is just for example)
//! let mut cpu = Cpu::new(memory, config, status);
//! 
//! // Run!
//! loop {
//!     let result = cpu.step();
//!     // do other stuff...
//! }
//!
//! ```
//! 
//! dg6502 implements both legal and illegal opcodes. dg6502 is unit tested via
//! individual opcode tests (~10,000 per opcode) and end-to-end tested with NEStest.
//! All fields of [Cpu] are public for you to read and write to if you need.
//! 
//! ## Implementing CPUMemory
//! If the BasicCPUMemory does not fit your needs (i.e you're looking for mirroring or
//! memory mapping), you'll want to implement the CPUMemory trait on a struct you'll use
//! for your memory.
//! 
//! Implementing CPUMemory is very simple as it's just two methods. read() takes in a u16 for
//! address and returns a u8, and write() takes in a u8 operand and u16 address to write to. Go ham!
//! 
//! ## Design
//! I used this project for my IB class' IA assignment. That assignment required me writing
//! about the design of this emulator, which you could read here if you'd like (COMING SOON).
//! 
//! ## Support
//! If you have any bugs, feedback, questions, issues, suggestions, etc., feel free to submit an issue to
//! the dg6502 repository.
//! 
mod cpu;
mod bus;

pub use crate::cpu::{Cpu, CpuConfig, CpuStepReturn, IllegalBehavior, JamBehavior, StatusRegister};
pub use crate::bus::{CPUMemory, BasicCPUMemory};