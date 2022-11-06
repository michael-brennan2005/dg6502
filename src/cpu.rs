use std::str::FromStr;

use ndarray::Array2;
use strum_macros::EnumString;

use crate::bus;

#[derive(EnumString, Debug)]
pub enum Instruction {
    /// add with carry
    ADC,
    /// and (with accumulator)
    AND,
    /// arithmetic shift left
    ASL,
    /// branch on carry clear
    BCC,
    /// branch on carry set
    BCS,
    /// branch on equal (zero set)
    BEQ,
    /// bit test
    BIT,
    /// branch on minus (negative set)
    BMI,
    /// branch on not equal (zero clear)
    BNE,
    /// branch on plus (negative clear)
    BPL,
    /// break/interrupt
    BRK,
    /// branch on overflow clear
    BVC,
    /// branch on overflow set
    BVS,
    /// clear carry
    CLC,
    /// clear decimal
    CLD,
    /// clear interrupt disable
    CLI,
    /// clear overflow
    CLV,
    /// compare (with accumulator)
    CMP,
    /// compare with X
    CPX,
    /// compare with Y
    CPY,
    /// decrement
    DEC,
    /// decrement X
    DEX,
    /// decrement Y
    DEY,
    /// exclusive or (with accumulator)
    EOR,
    /// increment
    INC,
    /// increment X
    INX,
    /// increment Y
    INY,
    /// jump
    JMP,
    /// jump subroutine
    JSR,
    /// load accumulator
    LDA,
    /// load x
    LDX,
    /// load y
    LDY,
    /// logical shift right
    LSR,
    /// no operation
    NOP,
    /// or with accumulator
    ORA,
    /// push accumulator
    PHA,
    /// push processor status (SR)
    PHP,
    /// pull accumulator
    PLA,
    /// pull processor status (SR)
    PLP,
    /// rotate left
    ROL,
    /// rotate right
    ROR,
    /// return from subroutine
    RTS,
    /// return from interrupt
    RTI,
    /// subtract with carry
    SBC,
    /// set carry
    SEC,
    /// set decimal
    SED,
    /// set interrupt disable
    SEI,
    /// store accumulator
    STA,
    /// store x
    STX,
    /// store y
    STY,
    /// transfer accumulator to X
    TAX,
    /// transfer accumulator to Y
    TAY,
    /// transfer stack pointer to X
    TSX,
    /// transfer X to accumulator
    TXA,
    /// transfer X to stack pointer
    TXS,
    /// transfer Y to accumulator
    TYA
}

#[derive(EnumString, Debug, Copy, Clone)]
pub enum AddressMode {
    #[strum(serialize="A")]
    Accumulator,
    #[strum(serialize="abs")]
    Absolute,
    #[strum(serialize="abs,X")]
    AbsoluteXIndex,
    #[strum(serialize="abs,Y")]
    AbsoluteYIndex,
    #[strum(serialize="#")]
    Immediate,
    #[strum(serialize="impl")]
    Implied,
    #[strum(serialize="ind")]
    Indirect,
    #[strum(serialize="X,ind")]
    IndirectXIndex,
    #[strum(serialize="ind,Y")]
    IndirectYIndex,
    #[strum(serialize="rel")]
    Relative,
    #[strum(serialize="zpg")]
    Zeropage,
    #[strum(serialize="zpg,X")]
    ZeropageXIndex,
    #[strum(serialize="zpg,Y")]
    ZeropageYIndex
}

#[derive(Debug, Clone, Copy)]
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

    fn to_u8(&self) -> u8 {
        (self.negative as u8) << 7 &
        (self.overflow as u8) << 6 &
        (0) << 5 & // ignored bit
        (self._break as u8) << 4 &
        (self.decimal as u8) << 3 &
        (self.interrupt as u8) << 2 &
        (self.zero as u8) << 1 &
        (self.carry as u8)
    }

    fn from_u8(x: u8) -> Self {
       StatusRegister {
           negative: (x & 0b10000000) >> 7 == 1,
           overflow: (x & 0b01000000) >> 6 == 1,
           _break: (x & 0b00100000) >> 5 == 1,
           decimal: (x & 0b00001000) >> 4 == 1,
           interrupt: (x & 0b00000100) >> 3 == 1,
           zero: (x & 0b00000010) >> 2 == 1,
           carry: (x & 0b00000001) == 1
       }
    }
}

pub struct Cpu<T: bus::Bus> {
    pub program_counter: u16,
    accumulator: u8,
    x: u8,
    y: u8,
    status: StatusRegister,
    stack_pointer: u8,
    bus: T,
    lookup_table: Array2<&'static str>
}

impl<T: bus::Bus> Cpu<T> {
    pub fn new(bus: T) -> Self {
        let lookup_table = ndarray::arr2(&[
            ["BRK impl","ORA X,ind", "---",   "---", "---", "ORA zpg", "ASL zpg", "---", "PHP impl", "ORA #", "ASL A", "---", "---", "ORA abs", "ASL abs", "---"],
            ["BPL rel", "ORA ind,Y", "---",   "---", "---", "ORA zpg,X", "ASL zpg,X", "---", "CLC impl", "ORA abs,Y", "---", "---", "---", "ORA abs,X", "ASL abs,X", "---"],
            ["JSR abs", "AND X,ind", "---",   "---", "BIT zpg", "AND zpg", "ROL zpg", "---", "PLP impl", "AND #", "ROL A", "---", "BIT abs", "AND abs", "ROL abs", "---"],
            ["BMI rel", "AND ind,Y", "---",   "---", "---", "AND zpg,X", "ROL zpg,X", "---", "SEC impl", "AND abs,Y", "---", "---", "---", "AND abs,X", "ROL abs,X", "---"],
            ["RTI impl","EOR X,ind", "---",   "---", "---", "EOR zpg", "LSR zpg", "---", "PHA impl", "EOR #", "LSR A", "---", "JMP abs", "EOR abs", "LSR abs", "---"],
            ["BVC rel", "EOR ind,Y", "---",   "---", "---", "EOR zpg,X", "LSR zpg,X", "---", "CLI impl", "EOR abs,Y", "---", "---", "---", "EOR abs,X", "LSR abs,X", "---"],
            ["RTS impl","ADC X,ind", "---",   "---", "---", "ADC zpg", "ROR zpg", "---", "PLA impl", "ADC #", "ROR A", "---", "JMP ind", "ADC abs", "ROR abs", "---"],
            ["BVS rel", "ADC ind,Y", "---",   "---", "---", "ADC zpg,X", "ROR zpg,X", "---", "SEI impl", "ADC abs,Y", "---", "---", "---", "ADC abs,X", "ROR abs,X", "---"],
            ["---",     "STA X,ind", "---",   "---", "STY zpg", "STA zpg", "STX zpg", "---", "DEY impl", "---", "TXA impl", "---", "STY abs", "STA abs", "STX abs", "---"],
            ["BCC rel", "STA ind,Y", "---",   "---", "STY zpg,X", "STA zpg,X", "STX zpg,Y", "---", "TYA impl", "STA abs,Y", "TXS impl", "---", "---", "STA abs,X", "---", "---"],
            ["LDY #",   "LDA X,ind", "LDX #", "---", "LDY zpg", "LDA zpg", "LDX zpg", "---", "TAY impl", "LDA #", "TAX impl", "---", "LDY abs", "LDA abs", "LDX abs", "---"],
            ["BCS rel", "LDA ind,Y", "---",   "---", "LDY zpg,X", "LDA zpg,X", "LDX zpg,Y", "---", "CLV impl", "LDA abs,Y", "TSX impl", "---", "LDY abs,X", "LDA abs,X", "LDX abs,Y", "---"],
            ["CPY #",   "CMP X,ind", "---",   "---", "CPY zpg", "CMP zpg", "DEC zpg", "---", "INY impl", "CMP #", "DEX impl", "---", "CPY abs", "CMP abs", "DEC abs", "---"],
            ["BNE rel", "CMP ind,Y", "---",   "---", "---", "CMP zpg,X", "DEC zpg,X", "---", "CLD impl", "CMP abs,Y", "---", "---", "---", "CMP abs,X", "DEC abs,X", "---"],
            ["CPX #",   "SBC X,ind", "---",   "---", "CPX zpg", "SBC zpg", "INC zpg", "---", "INX impl", "SBC #", "NOP impl", "---", "CPX abs", "SBC abs", "INC abs", "---"],
            ["BEQ rel", "SBC ind,Y", "---",   "---", "---", "SBC zpg,X", "INC zpg,X", "---", "SED impl", "SBC abs,Y", "---", "---", "---", "SBC abs,X", "INC abs,X", "---"]
        ]);

        Cpu {
            program_counter: 0,
            accumulator: 0,
            x: 0,
            y: 0,
            status: StatusRegister::new(),
            stack_pointer: 255,
            bus,
            lookup_table
        }
    }

    pub fn set_if_zero(&mut self, operand: u8) {
        if operand == 0 {
            self.status.zero = true;
        } else {
            self.status.zero = false;
        }
    }

    pub fn set_if_negative(&mut self, operand: u8) {
        if (operand & 0x80) >> 7 == 1 {
            self.status.negative = true;
        } else {
            self.status.negative = false;
        }
    }

    pub fn fetch_operand(&mut self, address_mode: AddressMode) -> (u8, u16) {
        self.bus.write(self.stack_pointer as u16 + 0x100, self.accumulator);
        self.stack_pointer -= 1;
        match address_mode {
            AddressMode::Accumulator => {
                (self.accumulator, 0)
            },
            AddressMode::Absolute => {
                let low_byte = self.bus.read(self.program_counter + 1);
                let high_byte = self.bus.read(self.program_counter + 2);
                self.program_counter += 2;
                let address = ((high_byte as u16) << 8) | low_byte as u16;
                (self.bus.read(address), address)
            },
            AddressMode::AbsoluteXIndex => {
                let low_byte = self.bus.read(self.program_counter + 1);
                let high_byte = self.bus.read(self.program_counter + 2);
                self.program_counter += 2;
                let address = (((high_byte as u16) << 8) | low_byte as u16) + (self.x as u16);
                (self.bus.read(address), address)
            },
            AddressMode::AbsoluteYIndex => {
                let low_byte = self.bus.read(self.program_counter + 1);
                let high_byte = self.bus.read(self.program_counter + 2);
                self.program_counter += 2;
                let address = (((high_byte as u16) << 8) | low_byte as u16) + (self.y as u16);
                (self.bus.read(address), address)
            },
            AddressMode::Immediate => {
                let byte = self.bus.read(self.program_counter + 1);
                self.program_counter += 1;
                (byte, self.program_counter)
            },
            AddressMode::Implied => {
                (0, 0)
            },
            AddressMode::Indirect => {
                let low_byte = self.bus.read(self.program_counter + 1);
                let high_byte = self.bus.read(self.program_counter + 2);
                self.program_counter += 2;
                (0, (high_byte as u16) << 8 | low_byte as u16)
            },
            AddressMode::IndirectXIndex => {
                let byte = self.bus.read(self.program_counter + 1);
                self.program_counter += 1;
                let low_byte = self.bus.read((byte + self.x) as u16);
                let high_byte = self.bus.read((byte + self.x + 1) as u16);
                let address = ((high_byte as u16) << 8) | low_byte as u16;

                (self.bus.read(address), address)
            },
            AddressMode::IndirectYIndex => {
                // fetch  address at zero page
                let zeropage_byte = self.bus.read(self.program_counter + 1);
                self.program_counter += 1;
                let low_byte = self.bus.read((zeropage_byte) as u16);
                let high_byte = self.bus.read((zeropage_byte + 1) as u16);
                let address = (((high_byte as u16) << 8) | low_byte as u16) + (self.y as u16);
                (self.bus.read(address), address)
            },
            AddressMode::Relative => {
                // this will be broken
                let byte = self.bus.read(self.program_counter + 1);
                self.program_counter += 1;
                let address = (self.program_counter as i16) + i16::from(byte);
                (0,address as u16)
            },
            AddressMode::Zeropage => {
                let low_byte = self.bus.read(self.program_counter + 1);
                self.program_counter += 1;
                (self.bus.read(low_byte as u16), self.program_counter)
            },
            AddressMode::ZeropageXIndex => {
                let low_byte = self.bus.read(self.program_counter + 1);
                self.program_counter += 1;
                (self.bus.read(low_byte as u16 + self.x as u16), low_byte as u16 + self.x as u16)
            },
            AddressMode::ZeropageYIndex => {
                let low_byte = self.bus.read(self.program_counter + 1);
                self.program_counter += 1;
                (self.bus.read(low_byte as u16 + self.y as u16), low_byte as u16 + self.y as u16)
            },
        }

    }

    pub fn reset(&mut self) {
        let high_byte = self.bus.read(0xFFFD);
        let low_byte = self.bus.read(0xFFFC);

        let new_address = (high_byte as u16) << 8 & (low_byte as u16);
        self.program_counter = new_address;
    }

    pub fn step(&mut self) {
        let opcode = self.bus.read(self.program_counter);

        let high_nibble = (opcode & 0xF0) >> 4;
        let low_nibble = opcode & 0x0F;

        let instruction = self.lookup_table.get([high_nibble as usize, low_nibble as usize]).unwrap().split(" ").collect::<Vec<&str>>();

        let opcode = Instruction::from_str(instruction[0]).unwrap();
        let address_mode = AddressMode::from_str(instruction[1]).unwrap();
        let (operand, address) = self.fetch_operand(address_mode);

        match opcode {
            Instruction::ADC => self.adc(address, operand),
            Instruction::AND => self.and(operand),
            Instruction::ASL => self.asl(address, operand, true),
            Instruction::BCC => self.bcc(address),
            Instruction::BCS => self.bcs(address),
            Instruction::BEQ => self.beq(address),
            Instruction::BIT => self.bit(operand),
            Instruction::BMI => self.bmi(address),
            Instruction::BNE => self.bne(address),
            Instruction::BPL => self.bpl(address),
            Instruction::BRK => self.brk(address),
            Instruction::BVC => self.bvc(address),
            Instruction::BVS => self.bvs(address),
            Instruction::CLC => self.clc(),
            Instruction::CLD => self.cld(),
            Instruction::CLI => self.cli(),
            Instruction::CLV => self.clv(),
            Instruction::CMP => self.cmp(operand),
            Instruction::CPX => self.cpx(operand),
            Instruction::CPY => self.cpy(operand),
            Instruction::DEC => self.dec(address, operand),
            Instruction::DEX => self.dex(),
            Instruction::DEY => self.dey(),
            Instruction::EOR => self.eor(operand),
            Instruction::INC => self.inc(address, operand),
            Instruction::INX => self.inx(),
            Instruction::INY => self.iny(),
            Instruction::JMP => self.jmp(address),
            Instruction::JSR => self.jsr(address),
            Instruction::LDA => self.lda(operand),
            Instruction::LDX => self.ldx(operand),
            Instruction::LDY => self.ldy(operand),
            Instruction::LSR => self.lsr(address, operand, true),
            Instruction::NOP => self.nop(),
            Instruction::ORA => self.ora(operand),
            Instruction::PHA => self.pha(),
            Instruction::PHP => self.php(),
            Instruction::PLA => self.pla(),
            Instruction::PLP => self.plp(),
            Instruction::ROL => self.rol(address, operand, true),
            Instruction::ROR => self.ror(address, operand, true),
            Instruction::RTS => self.rts(),
            Instruction::RTI => self.rti(),
            Instruction::SBC => self.sbc(address, operand),
            Instruction::SEC => self.sec(),
            Instruction::SED => self.sed(),
            Instruction::SEI => self.sei(),
            Instruction::STA => self.sta(address),
            Instruction::STX => self.stx(address),
            Instruction::STY => self.sty(address),
            Instruction::TAX => self.tax(),
            Instruction::TAY => self.tay(),
            Instruction::TSX => self.tsx(),
            Instruction::TXA => self.txa(),
            Instruction::TXS => self.txs(),
            Instruction::TYA => self.tya(),
        }
        self.program_counter += 1;
    }

    pub fn push_to_stack(&mut self, x: u8) {
        self.bus.write(self.stack_pointer as u16 + 0x100, x);
        self.stack_pointer -= 1;
    }

    pub fn pop_from_stack(&mut self) -> u8 {
        let x = self.bus.read(self.stack_pointer as u16 + 0x100);
        self.stack_pointer += 1;
        x
    }
    // Transfer instructions

    pub fn lda(&mut self, operand: u8) {
        self.accumulator = operand;
        self.set_if_negative(operand);
        self.set_if_zero(operand);
    }

    pub fn ldx(&mut self, operand: u8) {
        self.x = operand;
        self.set_if_negative(operand);
        self.set_if_zero(operand);
    }

    pub fn ldy(&mut self, operand: u8) {
        self.y = operand;
        self.set_if_negative(operand);
        self.set_if_zero(operand);
    }

    pub fn sta(&mut self, address: u16) {
        self.bus.write(address, self.accumulator);
    }

    pub fn stx(&mut self, address: u16) {
        self.bus.write(address, self.x);
    }

    pub fn sty(&mut self, address: u16) {
        self.bus.write(address, self.y);
    }

    pub fn tax(&mut self) {
        self.x = self.accumulator;
        self.set_if_negative(self.x);
        self.set_if_zero(self.x);
    }

    pub fn tay(&mut self) {
        self.y = self.accumulator;
        self.set_if_negative(self.y);
        self.set_if_zero(self.y);
    }

    pub fn tsx(&mut self) {
        self.x = self.stack_pointer;
        self.set_if_negative(self.x);
        self.set_if_zero(self.x);
    }

    pub fn txa(&mut self) {
        self.accumulator = self.x;
        self.set_if_negative(self.accumulator);
        self.set_if_zero(self.accumulator);
   }

    pub fn txs(&mut self) {
        self.stack_pointer = self.x;
    }

    pub fn tya(&mut self) {
        self.accumulator = self.y;
        self.set_if_negative(self.accumulator);
        self.set_if_zero(self.accumulator);
   }

    // Stack instructions
    pub fn pha(&mut self) {
        self.push_to_stack(self.accumulator);
        self.stack_pointer -= 1;
    }

    pub fn php(&mut self) {
        self.bus.write(self.stack_pointer as u16 + 0x100, self.status.to_u8() | (0b0011000));
        self.stack_pointer -= 1;
    }

    pub fn pla(&mut self) {
        self.stack_pointer += 1;
        let new_accumulator = self.bus.read(self.stack_pointer as u16 + 0x100);
        self.accumulator = new_accumulator;
        self.set_if_negative(new_accumulator);
        self.set_if_zero(new_accumulator);
    }


    pub fn plp(&mut self) {
        self.stack_pointer += 1;
        self.status = StatusRegister::from_u8(self.bus.read(self.stack_pointer as u16 + 0x100) & !(0b00110000));
    }

    // Decrements & increments
    pub fn dec(&mut self, address: u16, operand: u8) {
        self.bus.write(address, operand.wrapping_sub(1));
        self.set_if_negative(operand.wrapping_sub(1));
        self.set_if_zero(operand.wrapping_sub(1));
    }

    pub fn dex(&mut self) {
        self.x = self.x.wrapping_sub(1);
        self.set_if_negative(self.x);
        self.set_if_zero(self.x);
    }

    pub fn dey(&mut self) {
        self.y = self.y.wrapping_sub(1);
        self.set_if_negative(self.y);
        self.set_if_zero(self.y);
    }

    pub fn inc(&mut self, address: u16, operand: u8) {
        self.bus.write(address, operand.wrapping_add(1));
        self.set_if_negative(operand.wrapping_add(1));
        self.set_if_zero(operand.wrapping_add(1));
    }

   pub fn inx(&mut self) {
        self.x = self.x.wrapping_add(1);
        self.set_if_negative(self.x);
        self.set_if_zero(self.x);
    }

    pub fn iny(&mut self) {
        self.y = self.y.wrapping_add(1);
        self.set_if_negative(self.y);
        self.set_if_zero(self.y);
    }

    // Arithmetic operations
    pub fn adc(&mut self, operand: u8) {
        let result = (operand as u16) + (self.accumulator as u16);
        self.status.carry = !(0 <= result && result <= 255);
        self.status.overflow = -128 <= (result as i16) && (result as i16) <= 127;
        self.status.zero = result == 0;
        self.status.negative = (result as i8) < 0;
    }

    pub fn sbc(&mut self, operand: u8) {
        let result = (operand as i16) - (self.accumulator as i16);
        self.status.carry = 0 <= result && result <= 255;
        self.status.overflow = -128 <= (result as i16) && (result as i16) <= 127;
        self.status.zero = result == 0;
        self.status.negative = (result as i8) < 0;
    }

    // Logical operations
    pub fn and(&mut self, operand: u8) {
        self.accumulator &= operand;
        self.set_if_negative(self.accumulator);
        self.set_if_zero(self.accumulator);
    }

    pub fn ora(&mut self, operand: u8) {
        self.accumulator |= operand;
        self.set_if_negative(self.accumulator);
        self.set_if_zero(self.accumulator);
    }

    pub fn eor(&mut self, operand: u8) {
        self.accumulator ^= operand;
        self.set_if_negative(self.accumulator);
        self.set_if_zero(self.accumulator);
    }

    // Shift & rotate instructions
    pub fn asl(&mut self, address: u16, operand: u8, accumulator: bool) {
        if accumulator {
            let high_bit = (self.accumulator & 0b10000000) >> 7;
            self.status.carry = high_bit == 1;
            self.accumulator = self.accumulator << 1;
            self.set_if_negative(self.accumulator);
            self.set_if_zero(self.accumulator);
        } else {
            let high_bit = (operand & 0b10000000) >> 7;
            self.status.carry = high_bit == 1;
            let new_num = operand << 1;
            self.bus.write(address, new_num);
            self.set_if_negative(new_num);
            self.set_if_zero(new_num);
        }
    }

    pub fn lsr(&mut self, address: u16, operand: u8, accumulator: bool) {
        if accumulator {
            let low_bit = self.accumulator & 0b1;
            self.status.carry = low_bit == 1;
            self.accumulator = self.accumulator >> 1;
            self.set_if_negative(self.accumulator);
            self.set_if_zero(self.accumulator);
        } else {
            let low_bit = (operand & 0b10000000) >> 7;
            self.status.carry = low_bit == 1;
            let new_num = operand >> 1;
            self.bus.write(address, new_num);
            self.set_if_negative(new_num);
            self.set_if_zero(new_num);
        }
    }

    pub fn rol(&mut self, address: u16, operand: u8, accumulator: bool) {
        if accumulator {
            let high_bit = (self.accumulator & 0b10000000) >> 7;
            self.status.carry = high_bit == 1;
            self.accumulator = (self.accumulator << 1) | high_bit;
            self.set_if_negative(self.accumulator);
            self.set_if_zero(self.accumulator);
        } else {
            let high_bit = (operand & 0b10000000) >> 7;
            self.status.carry = high_bit == 1;
            let new_num = operand << 1;
            self.bus.write(address, new_num);
            self.set_if_negative(new_num);
            self.set_if_zero(new_num);
        }
    }

    pub fn ror(&mut self, address: u16, operand: u8, accumulator: bool) {
        if accumulator {
            let low_bit = self.accumulator & 0b1;
            self.status.carry = low_bit == 1;
            self.accumulator = (self.accumulator >> 1) | (low_bit << 7);
            self.set_if_negative(self.accumulator);
            self.set_if_zero(self.accumulator);
        } else {
            let low_bit = operand & 0b1;
            self.status.carry = low_bit == 1;
            let new_num = (operand >> 1) | (low_bit << 7);
            self.bus.write(address, new_num);
            self.set_if_negative(new_num);
            self.set_if_zero(new_num);
        }
    }

    // Flag instructions
    pub fn clc(&mut self) {
        self.status.carry = false;
    }

    pub fn cld(&mut self) {
        self.status.decimal = false;
    }

    pub fn cli(&mut self) {
        self.status.interrupt = false;
    }

    pub fn clv(&mut self) {
        self.status.overflow = false;
    }

    pub fn sec(&mut self) {
        self.status.carry = true;
    }

    pub fn sed(&mut self) {
        self.status.decimal = true;
    }

    pub fn sei(&mut self) {
       self.status.interrupt = true;
    }

    // Comparisons
    pub fn cmp(&mut self, operand: u8) {
        todo!()
    }

    pub fn cpx(&mut self, operand: u8) {
        todo!()
    }

    pub fn cpy(&mut self, operand: u8) {
        todo!()
    }

    // Branch instructions
    pub fn bcc(&mut self, address: u16) {
        if !self.status.carry {
            self.program_counter = address;
        }
    }

    pub fn bcs(&mut self, address: u16) {
        if self.status.carry {
            self.program_counter = address;
        }
    }

    pub fn beq(&mut self, address: u16) {
        if self.status.zero {
            self.program_counter = address;
        }
    }

    pub fn bmi(&mut self, address: u16) {
        if self.status.negative {
            self.program_counter = address;
        }
    }

    pub fn bne(&mut self, address: u16) {
        if !self.status.zero {
            self.program_counter = address;
        }
    }

    pub fn bpl(&mut self, address: u16) {
        if !self.status.negative {
            self.program_counter = address;
        }
    }

    pub fn bvc(&mut self, address: u16) {
        if !self.status.overflow {
            self.program_counter = address;
        }
    }

    pub fn bvs(&mut self, address: u16) {
        if self.status.overflow {
            self.program_counter = address;
        }
    }

    // Jump instructions
    pub fn jmp(&mut self, address: u16) {
        self.program_counter = address;
    }

    pub fn jsr(&mut self, address: u16) {
        let high_byte = ((self.program_counter + 2) >> 8) as u8;
        let low_byte = ((self.program_counter + 2) & 0b00001111) as u8;
        self.push_to_stack(high_byte);
        self.push_to_stack(low_byte);

        self.program_counter = address;
    }

    pub fn rts(&mut self) {
        let low_byte = self.pop_from_stack();
        let high_byte = self.pop_from_stack();
        self.program_counter = ((high_byte as u16) << 8) | (low_byte as u16);
    }

    // Interrupts
    pub fn brk(&mut self, address: u16) {
        self.program_counter += 2;
        let high_byte = (self.program_counter >> 8) as u8;
        let low_byte = (self.program_counter & 0xFF) as u8;
        self.push_to_stack(high_byte);
        self.push_to_stack(low_byte);
        self.status._break = true;
        self.push_to_stack(self.status.to_u8());

        let new_high_byte = self.bus.read(0xFFFF);
        let new_low_byte = self.bus.read(0xFFFE);
        let pc = ((new_high_byte as u16) << 8) | new_low_byte as u16;
        self.program_counter = pc;
    }

    pub fn rti(&mut self) {
        let mut status = StatusRegister::from_u8(self.pop_from_stack());
        status._break = false;

        let low_byte = self.pop_from_stack();
        let high_byte = self.pop_from_stack();
        let program_counter = ((high_byte as u16) << 8) | low_byte as u16;

        self.status = status;
        self.program_counter = program_counter;
    }

    // Other
    pub fn bit(&mut self, operand: u8) {
        self.status.negative = (operand & 0b10000000 >> 7) == 1;
        self.status.overflow = (operand & 0b01000000 >> 6) == 1;

        self.status.zero = (operand & self.accumulator) > 0;
    }

    pub fn nop(&mut self) {
        // its noping time!
    }
}
