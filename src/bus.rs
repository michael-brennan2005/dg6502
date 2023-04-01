use std::fs::{self};
use std::io::{Error, ErrorKind};

/// The [Cpu](crate::Cpu) takes in a struct with a CPUMemory trait for dealing with memory reads and writes. Implement
/// this trait if you would like to customize how reading or writing works.
/// If your memory is just a 64KB buffer with no mapping/mirroring, you may want to use [BasicCPUMemory](crate::BasicCPUMemory).
pub trait CPUMemory {
    /// Reading from an address.
    fn read(&self, address: u16) -> u8;
    /// Writing from an address.
    fn write(&mut self, address: u16, data: u8);
}

/// Memory is one contiguous 64KB buffer with no mapping or mirroring.
pub struct BasicCPUMemory {
    buffer: Vec<u8>
}

impl BasicCPUMemory {
    /// For creating a memory buffer from a file. Specify an offset if you would like
    /// your data to be loaded in from a specific start point; if you set this to 0,
    /// it will begin loading data at 0x0.
    pub fn from_file(path: &String, offset: u16) -> Result<Self, std::io::Error> {
        match fs::read(path) {
            Ok(mut buffer) => {
                if buffer.len() > (65536 - offset as usize) {
                    Err(Error::new(ErrorKind::Other, "File is too large to be loaded into 64KB buffer with current offset."))
                } else {
                    let mut memory: Vec<u8> = vec![0; 65536];
                    let start = offset;
                    for i in 0..buffer.len() {
                        memory[i + start as usize] = memory[i];
                    }
                    buffer.resize(65536, 0);
                    Ok(BasicCPUMemory {
                        buffer: memory
                    })
                }
            },
            Err(err) => {
                Err(err)
            }
        }

    }
}
impl Default for BasicCPUMemory {
    fn default() -> Self {
        BasicCPUMemory { buffer: vec![0; 65536] }
    }
}


impl TryFrom<Vec<u8>> for BasicCPUMemory {
    type Error = ();

    /// If you already have an existing Vec<u8> and would like to use it for your CPU memory.
    /// If your Vec<u8> is too large this will error. If your Vec<u8> is too small it will
    /// upsize your vector and fill in the unused memory with 0.
    fn try_from(value: Vec<u8>) -> Result<Self, Self::Error> {
        if value.len() > 65536 {
            Err(())
        } else {
            let mut buffer: Vec<u8> = vec![0; 65536];
            for (index, ele) in value.iter().enumerate() {
                buffer[index] = *ele;
            }
            Ok(BasicCPUMemory { buffer })
        }
    }
}

impl CPUMemory for BasicCPUMemory {
    fn read(&self, address: u16) -> u8 {
        self.buffer[address as usize]
    }

    fn write(&mut self, address: u16, data: u8) {
        self.buffer[address as usize] = data;
    }
}