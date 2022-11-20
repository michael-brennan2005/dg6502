/// This is the type for how the CPU retrives things from memory.
pub trait Bus {
    fn read(&self, address: u16) -> u8;
    fn write(&mut self, address: u16, data: u8);
}

/// Memory is just one big buffer, no mapping or anything.
pub struct BasicBus {
    buffer: Vec<u8>
}


impl TryFrom<Vec<u8>> for BasicBus {
    type Error = ();

    fn try_from(value: Vec<u8>) -> Result<Self, Self::Error> {
        if value.len() > 65536 {
            Err(())
        } else {
            let mut buffer: Vec<u8> = vec![0; 65536];
            for (index, ele) in value.iter().enumerate() {
                buffer[index] = *ele;
            }
            Ok(BasicBus { buffer })
        }
    }
}

impl Bus for BasicBus {
    fn read(&self, address: u16) -> u8 {
        self.buffer[address as usize]
    }

    fn write(&mut self, address: u16, data: u8) {
        self.buffer[address as usize] = data;
    }
}

impl BasicBus {
    pub fn swap_buffer(&mut self, new_buffer: Vec<u8>) { 
        let mut buffer: Vec<u8> = vec![0; 65536];
        for (index, ele) in new_buffer.iter().enumerate() {
            buffer[index] = *ele;
        }
        self.buffer = buffer;
    }
}
