use std::fs::{File, self};
use std::io::Read;

/// This is the type for how the CPU retrives things from memory.
pub trait CPUMemory {
    fn read(&self, address: u16) -> u8;
    fn write(&mut self, address: u16, data: u8);
}

/// Memory is just one big buffer, no mapping or anything.
pub struct BasicCPUMemory {
    buffer: Vec<u8>
}

impl BasicCPUMemory {
    pub fn from_file(path: &String) -> Result<Self, ()> {
        match fs::read(path) {
            Ok(mut buffer) => {
                if buffer.len() > 65536 {
                    Err(())
                } else {
                    buffer.resize(65536, 0);
                    Ok(BasicCPUMemory {
                        buffer
                    })
                }
            },
            Err(_) => {
                Err(())
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