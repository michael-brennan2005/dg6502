use std::str::FromStr;

use ndarray::Array2;
use strum_macros::EnumString;

use crate::bus;

#[derive(EnumString, Debug)]
pub enum Instruction {
    ADC,
    AND,
    ASL,
    BCC,
    BCS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    JMP,
    JSR,
    LDA,
    LDX,
    LDY,
    LSR,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    ROL,
    ROR,
    RTS,
    RTI,
    SBC,
    SEC,
    SED,
    SEI,
    STA,
    STX,
    STY,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
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
            ["BRK impl", "ORA X,ind", "---", "---", "---", "ORA zpg", "ASL zpg", "---", "PHP impl", "ORA #", "ASL A", "---", "---", "ORA abs", "ASL abs", "---"],
            ["BPL rel", "ORA ind,Y", "---", "---", "---", "ORA zpg,X", "ASL zpg,X", "---", "CLC impl", "ORA abs,Y", "---", "---", "---", "ORA abs,X", "ASL abs,X", "---"],
            ["JSR abs", "AND X,ind", "---", "---", "BIT zpg", "AND zpg", "ROL zpg", "---", "PLP impl", "AND #", "ROL A", "---", "BIT abs", "AND abs", "ROL abs", "---"],
            ["BMI rel", "AND ind,Y", "---", "---", "---", "AND zpg,X", "ROL zpg,X", "---", "SEC impl", "AND abs,Y", "---", "---", "---", "AND abs,X", "ROL abs,X", "---"],
            ["RTI impl", "EOR X,ind", "---", "---", "---", "EOR zpg", "LSR zpg", "---", "PHA impl", "EOR #", "LSR A", "---", "JMP abs", "EOR abs", "LSR abs", "---"],
            ["BVC rel", "EOR ind,Y", "---", "---", "---", "EOR zpg,X", "LSR zpg,X", "---", "CLI impl", "EOR abs,Y", "---", "---", "---", "EOR abs,X", "LSR abs,X", "---"],
            ["RTS impl", "ADC X,ind", "---", "---", "---", "ADC zpg", "ROR zpg", "---", "PLA impl", "ADC #", "ROR A", "---", "JMP ind", "ADC abs", "ROR abs", "---"],
            ["BVS rel", "ADC ind,Y", "---", "---", "---", "ADC zpg,X", "ROR zpg,X", "---", "SEI impl", "ADC abs,Y", "---", "---", "---", "ADC abs,X", "ROR abs,X", "---"],
            ["---",    "STA X,ind", "---", "---", "STY zpg", "STA zpg", "STX zpg", "---", "DEY impl", "---", "TXA impl", "---", "STY abs", "STA abs", "STX abs", "---"],
            ["BCC rel", "STA ind,Y", "---", "---", "STY zpg,X", "STA zpg,X", "STX zpg,Y", "---", "TYA impl", "STA abs,Y", "TXS impl", "---", "---", "STA abs,X", "---", "---"],
            ["LDY #", "LDA X,ind", "LDX #", "---", "LDY zpg", "LDA zpg", "LDX zpg", "---", "TAY impl", "LDA #", "TAX impl", "---", "LDY abs", "LDA abs", "LDX abs", "---"],
            ["BCS rel", "LDA ind,Y", "---", "---", "LDY zpg,X", "LDA zpg,X", "LDX zpg,Y", "---", "CLV impl", "LDA abs,Y", "TSX impl", "---", "LDY abs,X", "LDA abs,X", "LDX abs,Y", "---"],
            ["CPY #", "CMP X,ind", "---", "---", "CPY zpg", "CMP zpg", "DEC zpg", "---", "INY impl", "CMP #", "DEX impl", "---", "CPY abs", "CMP abs", "DEC abs", "---"],
            ["BNE rel", "CMP ind,Y", "---", "---", "---", "CMP zpg,X", "DEC zpg,X", "---", "CLD impl", "CMP abs,Y", "---", "---", "---", "CMP abs,X", "DEC abs,X", "---"],
            ["CPX #", "SBC X,ind", "---", "---", "CPX zpg", "SBC zpg", "INC zpg", "---", "INX impl", "SBC #", "NOP impl", "---", "CPX abs", "SBC abs", "INC abs", "---"],
            ["BEQ rel", "SBC ind,Y", "---", "---", "---", "SBC zpg,X", "INC zpg,X", "---", "SED impl", "SBC abs,Y", "---", "---", "---", "SBC abs,X", "INC abs,X", "---"]
        ]);

        Cpu {
            program_counter: 0,
            accumulator: 0,
            x: 0,
            y: 0,
            status: StatusRegister::new(),
            stack_pointer: 0,
            bus,
            lookup_table
        }
    }

    pub fn fetch_operand(&mut self, address_mode: AddressMode) -> u8 {
        match address_mode {
            AddressMode::Accumulator => {
                self.accumulator
            },
            AddressMode::Absolute => {
                let low_byte = self.bus.read(self.program_counter + 1);
                let high_byte = self.bus.read(self.program_counter + 2);
                self.program_counter += 2;
                let address = ((high_byte as u16) << 8) | low_byte as u16;
                self.bus.read(address)
            },
            AddressMode::AbsoluteXIndex => {
                 let low_byte = self.bus.read(self.program_counter + 1);
                let high_byte = self.bus.read(self.program_counter + 2);
                self.program_counter += 2;
                let address = (((high_byte as u16) << 8) | low_byte as u16) + (self.x as u16);
                self.bus.read(address)

            },
            AddressMode::AbsoluteYIndex => {
                let low_byte = self.bus.read(self.program_counter + 1);
                let high_byte = self.bus.read(self.program_counter + 2);
                self.program_counter += 2;
                let address = (((high_byte as u16) << 8) | low_byte as u16) + (self.y as u16);
                self.bus.read(address)
            },
            AddressMode::Immediate => {
                let byte = self.bus.read(self.program_counter + 1);
                self.program_counter += 1;
                byte
            },
            AddressMode::Implied => {
                0
            },
            // only JMP uses this, and it does some weird 16 bit value thing. Do that in the instruction
            AddressMode::Indirect => {
                0
            },
            AddressMode::IndirectXIndex => {
                let byte = self.bus.read(self.program_counter + 1);
                self.program_counter += 1;
                let low_byte = self.bus.read((byte + self.x) as u16);
                let high_byte = self.bus.read((byte + self.x + 1) as u16);
                let address = ((high_byte as u16) << 8) | low_byte as u16;

                self.bus.read(address)
            },
            AddressMode::IndirectYIndex => {
                // fetch  address at zero page
                let zeropage_byte = self.bus.read(self.program_counter + 1);
                self.program_counter += 1;
                let low_byte = self.bus.read((zeropage_byte) as u16);
                let high_byte = self.bus.read((zeropage_byte + 1) as u16);
                let address = (((high_byte as u16) << 8) | low_byte as u16) + (self.y as u16);
                self.bus.read(address)
            },
            // only BRANCH functions use this, and it returns 16 bit, do it instruction
            AddressMode::Relative => {
                0
            },
            AddressMode::Zeropage => {
                let low_byte = self.bus.read(self.program_counter + 1);
                self.program_counter += 1;
                self.bus.read(low_byte as u16)
            },
            AddressMode::ZeropageXIndex => {
                let low_byte = self.bus.read(self.program_counter + 1);
                self.program_counter += 1;
                self.bus.read(low_byte as u16 + self.x as u16)
            },
            AddressMode::ZeropageYIndex => {
                let low_byte = self.bus.read(self.program_counter + 1);
                self.program_counter += 1;
                self.bus.read(low_byte as u16 + self.y as u16)
            },
        }

    }

    pub fn step(&mut self) {
        let opcode = self.bus.read(self.program_counter);

        let high_nibble = (opcode & 0xF0) >> 4;
        let low_nibble = opcode & 0x0F;

        let instruction = self.lookup_table.get([high_nibble as usize, low_nibble as usize]).unwrap().split(" ").collect::<Vec<&str>>();

        let opcode = Instruction::from_str(instruction[0]).unwrap();
        let address_mode = AddressMode::from_str(instruction[1]).unwrap();
        self.fetch_operand(address_mode);
        println!("{:?} with address mode {:?}", opcode, address_mode);

        self.program_counter += 1;
    }
}
