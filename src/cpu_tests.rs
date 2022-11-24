#[cfg(test)]
mod cpu_tests {
    use std::{fs::read_to_string, fmt::Display, time::{Instant, Duration}};
    use serde::{Deserialize, Serialize};

    use crate::{cpu::{Cpu, StatusRegister}, bus::{BasicBus, Bus}};

    const NOT_ARITHMETIC_VALID_OPCODES: [&str; 131] = [
        "00", "10", "20", "30", "40", "50", "60", "70", "A0", "B0", "C0", "D0", "E0", "F0", "01", "11", "21", "31", 
        "41", "51","81", "91", "A1", "B1", "C1", "D1",  "A2", "24", "94", "A4", "B4", "C4", 
        "E4", "05", "15", "25", "35", "45", "55", "85", "95", "A5", "B5", "C5", "D5","06", "16" ,
        "26", "36", "46", "56", "66", "76", "86","96", "A6", "B6", "C6", "D6", "E6", "F6", "08", "18", "28", "38", "48", 
        "58", "68", "78", "88", "98", "A8", "B8", "C8", "D8", "E8", "F8", "09", "19", "29", "39", "49", "59",
        "99", "A9", "B9", "C9", "D9", "0A", "2A", "4A", "6A", "8A", "9A", "AA", "BA", "CA", "EA", "2C", "4C",
        "6C", "8C", "AC", "BC", "CC", "EC", "0D", "1D", "2D", "3D", "4D", "5D",  "8D", "9D", "AD", "BD", "CD", 
        "0E", "1E", "2E", "3E", "4E", "5E", "6E", "7E", "8E", "AE", "BE", "CE", "EE", "FE"
    ];

    const ARITHMETIC_VALID_OPCODES: [&str; 16] = [
        "61", "71", "E1", "F1", "65", "75", "E5", "F5", "69", "79", "E9", "F9", "6D", "7D", "ED", "FD",
    ];

    #[derive(Serialize, Deserialize)]
    struct OpcodeTest {
        name: String,
        initial: CpuState,
        r#final: CpuState
    }

    #[derive(Serialize, Deserialize, PartialEq, Eq)]
    struct CpuState {
        pc: u16,
        s: u8,
        a: u8,
        x: u8,
        y: u8,
        p: u8, // status
        ram: Vec<(u16, u8)>
    }

    impl Display for CpuState {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "PC: {}\nS: {}\nA: {}\nX: {}\nY: {}\nP: {}\nRAM: {}", self.pc, self.s, self.a, self.x, self.y, self.p, {
                let mut ram = String::new();
                for (address, data) in &self.ram {
                    ram += format!("({}, {}) ", address, data).as_str();
                }
                ram
            })
        }
    }

    fn test_fail(test: OpcodeTest, actual: CpuState) {
        println!("Test {} has failed.", test.name);
        println!("Initial:\n{}", test.initial);
        println!("Final (expected):\n{}", test.r#final);
        println!("Final (actual):\n{}", actual);
        panic!();
    }

    fn run_test(test: OpcodeTest, panic: bool) -> (bool, u8) {
        let mut cpu: Cpu<BasicBus> = Cpu::new(BasicBus::default());
        cpu.program_counter = test.initial.pc;
        cpu.stack_pointer = test.initial.s;
        cpu.accumulator = test.initial.a;
        cpu.x = test.initial.x;
        cpu.y = test.initial.y;
        cpu.status = StatusRegister::from_u8(test.initial.p);

        for (address, data) in &test.initial.ram {
            cpu.bus.write(*address, *data)
        }

        let cycles = cpu.step();
        let actual = CpuState {
            pc: cpu.program_counter,
            s: cpu.stack_pointer,
            a: cpu.accumulator,
            x: cpu.x,
            y: cpu.y,
            p: cpu.status.to_u8(),
            ram: {
                let mut vec: Vec<(u16, u8)> = vec![];
                for (address, _) in &test.r#final.ram {
                    vec.push((*address, cpu.bus.read(*address)));
                }
                vec
            }
        };

        if actual != test.r#final {
            if panic {
                test_fail(test, actual);
            }
            (false, cycles)
        } else {
            (true, cycles)
        }
    }

    fn run_and_time_test(test: OpcodeTest, panic: bool) -> (bool, Duration, u8) {
        let mut cpu: Cpu<BasicBus> = Cpu::new(BasicBus::default());
        cpu.program_counter = test.initial.pc;
        cpu.stack_pointer = test.initial.s;
        cpu.accumulator = test.initial.a;
        cpu.x = test.initial.x;
        cpu.y = test.initial.y;
        cpu.status = StatusRegister::from_u8(test.initial.p);

        for (address, data) in &test.initial.ram {
            cpu.bus.write(*address, *data)
        }

        let now = Instant::now();
        let cycles = cpu.step();
        let elapsed = now.elapsed();
        
        let actual = CpuState {
            pc: cpu.program_counter,
            s: cpu.stack_pointer,
            a: cpu.accumulator,
            x: cpu.x,
            y: cpu.y,
            p: cpu.status.to_u8(),
            ram: {
                let mut vec: Vec<(u16, u8)> = vec![];
                for (address, _) in &test.r#final.ram {
                    vec.push((*address, cpu.bus.read(*address)));
                }
                vec
            }
        };

        if actual != test.r#final {
            if panic {
                test_fail(test, actual);
            }
            (false, elapsed, cycles)
        } else {
            (true, elapsed, cycles)
        }
    }

    #[test]
    fn test_non_arithmetic_valid_opcodes() {
        for opcode in NOT_ARITHMETIC_VALID_OPCODES {
            let tests = read_to_string(format!("test_json/{}.json", opcode.to_lowercase())).unwrap();
            let tests: Vec<OpcodeTest> = serde_json::from_str(&tests).unwrap();

            println!("Beginning tests for opcode {}...", opcode);
            for test in tests {
                run_test(test, true);
            }
            println!("Tests for opcode {} have passed.", opcode);
            
        }
    }

    #[test]
    fn test_arithmetic_valid_opcodes() {
        for opcode in ARITHMETIC_VALID_OPCODES {
            let tests = read_to_string(format!("test_json/{}.json", opcode)).unwrap();
            let tests: Vec<OpcodeTest> = serde_json::from_str(&tests).unwrap();

            println!("Beginning tests for opcode {}...", opcode);
            for test in tests {
                run_test(test, true);
            }
            println!("Tests for opcode {} have passed.", opcode);
            
        }
    }

    #[test]
    fn grade_non_arithmetic_valid_opcodes() {
        let mut passed = 0;
        let mut failed = 0;
        let mut duration = Duration::new(0, 0);
        let mut cycles: usize = 0;
        for opcode in NOT_ARITHMETIC_VALID_OPCODES {
            let tests = read_to_string(format!("test_json/{}.json", opcode.to_lowercase())).unwrap();
            let tests: Vec<OpcodeTest> = serde_json::from_str(&tests).unwrap();

            println!("Beginning tests for opcode {}...", opcode);
            for test in tests {
                let (success, time_taken, cycles_taken) = run_and_time_test(test, false);
                if success {
                    passed += 1;
                } else {
                    failed += 1;
                };
                duration += time_taken;
                cycles += cycles_taken as usize;
            }
            println!("Tests for opcode {} have finished.", opcode);
            
        }
        println!("Total: {}, Passed: {}, Failed: {}, Grade: {}%", passed + failed, passed, failed, (passed as f32 / (passed as f32 + failed as f32)) * 100.0);
        println!("Total Time: {:?}, Time/Step: {:?}", duration, duration / (passed + failed));
        println!("Total Cycles: {:?}, Lower Bound Max Clock Rate: {:?} Hz", cycles, cycles as f64 / duration.as_secs_f64());
    }

    #[test]
    fn grade_arithmetic_valid_opcodes() {
        let mut passed = 0;
        let mut failed = 0;
        for opcode in ARITHMETIC_VALID_OPCODES {
            let tests = read_to_string(format!("test_json/{}.json", opcode)).unwrap();
            let tests: Vec<OpcodeTest> = serde_json::from_str(&tests).unwrap();

            println!("Beginning tests for opcode {}...", opcode);
            for test in tests {
                let (success, _) = run_test(test, false);
                if success {
                    passed += 1;
                } else {
                    failed += 1;
                };
            }
            println!("Tests for opcode {} have finished.", opcode);
            
        }
        println!("Total: {}, Passed: {}, Failed: {}, Grade: {}%", passed + failed, passed, failed, (passed as f32 / (passed as f32 + failed as f32)) * 100.0);
    }
}
