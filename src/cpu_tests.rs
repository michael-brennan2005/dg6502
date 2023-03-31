#[cfg(test)]
mod cpu_tests {
    use std::fs;

    use crate::{bus::{BasicCPUMemory, CPUMemory}, cpu::{StatusRegister, CpuConfig, Cpu}};

    #[test]
    fn nestest() {
        let nestest = fs::read("test_rom/nestest.nes").unwrap();

        let mut memory_vec = vec![0; 65536];
        let start = 0xC000;
        let length = 16384;
        
        for i in 0..length {
            memory_vec[start + i] = nestest[0x10 + i];
        }
    
        let nestest_mem = BasicCPUMemory::try_from(memory_vec).unwrap();
        let mut status = StatusRegister::new();
        status.ignored = true;
        status.interrupt = true;
        let mut cpu = Cpu::new(nestest_mem, CpuConfig { decimal_mode: false }, status);
    
        cpu.program_counter = 0xC000;
        let mut steps: usize = 0;
        while steps < 8991 {
            cpu.step();
            steps += 1;
            if cpu.bus.read(0x2) != 0 || cpu.bus.read(0x3) != 0 {
                println!("Addresses 0x2 and 0x3 have been set to: {:#X} {:#X}", cpu.bus.read(0x2), cpu.bus.read(0x3));
                panic!()
            }
        }
    }
}
