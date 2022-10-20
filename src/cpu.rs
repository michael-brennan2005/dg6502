use crate::bus;

struct StatusRegister {
    negative: bool,
    overflow: bool,
    _break: bool,
    decimal: bool,
    interrupt: bool,
    zero: bool,
    carry: bool
}

impl StatusRegister {
    fn new() -> Self {
        StatusRegister { negative: false, overflow: false, _break: false, decimal: false, interrupt: false, zero: false, carry: false }
    }
}

impl Into<u8> for StatusRegister {
    fn into(self) -> u8 {
        (self.negative as u8) << 7 &
        (self.overflow as u8) << 6 &
        (0) << 5 & // ignored bit
        (self._break as u8) << 4 &
        (self.decimal as u8) << 3 &
        (self.interrupt as u8) << 2 &
        (self.zero as u8) << 1 &
        (self.carry as u8)
    }
}

struct Cpu<T: bus::Bus> {
    program_counter: u16,
    accumulator: u16,
    x: u16,
    y: u16,
    status: StatusRegister,
    stack_pointer: u8,
    bus: T
}

impl<T: bus::Bus> Cpu<T> {
    pub fn new(bus: T) -> Self {
        Cpu {
            program_counter: 0,
            accumulator: 0,
            x: 0,
            y: 0,
            status: StatusRegister::new(),
            stack_pointer: 0,
            bus
        }
    }

    pub fn step(&mut self) {
        let opcode = self.bus.read(self.program_counter);
        let a = opcode & (0b11100000) >> 5;
        let b = opcode & (0b00011100) >> 2;
        let c = opcode & (0b00000011);

        match (a, b, c) {
            _ => {
                eprintln!("oh noesy woesy!");
            }
        }
    }
}
