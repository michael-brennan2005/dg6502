use std::fmt::Display;

use crate::bus;


const LOOKUP_TABLE: [(Instruction, AddressMode); 256] = [
    (Instruction::BRK, AddressMode::Implied),  (Instruction::ORA, AddressMode::IndirectXIndex), (Instruction::JAM, AddressMode::Accumulator),   (Instruction::SLO, AddressMode::IndirectXIndex), (Instruction::NOP, AddressMode::Zeropage), (Instruction::ORA, AddressMode::Zeropage), (Instruction::ASL, AddressMode::Zeropage), (Instruction::SLO, AddressMode::Zeropage), (Instruction::PHP, AddressMode::Implied), (Instruction::ORA, AddressMode::Immediate), (Instruction::ASL, AddressMode::Accumulator), (Instruction::ANC, AddressMode::Immediate), (Instruction::NOP, AddressMode::Absolute), (Instruction::ORA, AddressMode::Absolute), (Instruction::ASL, AddressMode::Absolute), (Instruction::SLO, AddressMode::Absolute),
    (Instruction::BPL, AddressMode::Relative), (Instruction::ORA, AddressMode::IndirectYIndex), (Instruction::JAM, AddressMode::Accumulator),   (Instruction::SLO, AddressMode::IndirectYIndex), (Instruction::NOP, AddressMode::ZeropageXIndex), (Instruction::ORA, AddressMode::ZeropageXIndex), (Instruction::ASL, AddressMode::ZeropageXIndex), (Instruction::SLO, AddressMode::ZeropageXIndex), (Instruction::CLC, AddressMode::Implied), (Instruction::ORA, AddressMode::AbsoluteYIndex), (Instruction::NOP, AddressMode::Implied), (Instruction::SLO, AddressMode::AbsoluteYIndex), (Instruction::NOP, AddressMode::AbsoluteXIndex), (Instruction::ORA, AddressMode::AbsoluteXIndex), (Instruction::ASL, AddressMode::AbsoluteXIndex), (Instruction::SLO, AddressMode::AbsoluteXIndex),
    (Instruction::JSR, AddressMode::Absolute), (Instruction::AND, AddressMode::IndirectXIndex), (Instruction::JAM, AddressMode::Accumulator),   (Instruction::RLA, AddressMode::IndirectXIndex), (Instruction::BIT, AddressMode::Zeropage), (Instruction::AND, AddressMode::Zeropage), (Instruction::ROL, AddressMode::Zeropage),                   (Instruction::RLA, AddressMode::Zeropage), (Instruction::PLP, AddressMode::Implied), (Instruction::AND, AddressMode::Immediate), (Instruction::ROL, AddressMode::Accumulator), (Instruction::ANC, AddressMode::Immediate), (Instruction::BIT, AddressMode::Absolute), (Instruction::AND, AddressMode::Absolute), (Instruction::ROL, AddressMode::Absolute), (Instruction::RLA, AddressMode::Absolute),
    (Instruction::BMI, AddressMode::Relative), (Instruction::AND, AddressMode::IndirectYIndex), (Instruction::JAM, AddressMode::Accumulator),   (Instruction::RLA, AddressMode::IndirectYIndex), (Instruction::NOP, AddressMode::ZeropageXIndex), (Instruction::AND, AddressMode::ZeropageXIndex), (Instruction::ROL, AddressMode::ZeropageXIndex), (Instruction::RLA, AddressMode::ZeropageXIndex), (Instruction::SEC, AddressMode::Implied), (Instruction::AND, AddressMode::AbsoluteYIndex), (Instruction::NOP, AddressMode::Implied), (Instruction::RLA, AddressMode::AbsoluteYIndex), (Instruction::NOP, AddressMode::AbsoluteXIndex), (Instruction::AND, AddressMode::AbsoluteXIndex), (Instruction::ROL, AddressMode::AbsoluteXIndex), (Instruction::RLA, AddressMode::AbsoluteXIndex),
    (Instruction::RTI, AddressMode::Implied),  (Instruction::EOR, AddressMode::IndirectXIndex), (Instruction::JAM, AddressMode::Accumulator),   (Instruction::SRE, AddressMode::IndirectXIndex), (Instruction::NOP, AddressMode::Zeropage), (Instruction::EOR, AddressMode::Zeropage), (Instruction::LSR, AddressMode::Zeropage),                     (Instruction::SRE, AddressMode::Zeropage), (Instruction::PHA, AddressMode::Implied), (Instruction::EOR, AddressMode::Immediate), (Instruction::LSR, AddressMode::Accumulator), (Instruction::ALR, AddressMode::Immediate), (Instruction::JMP, AddressMode::Absolute), (Instruction::EOR, AddressMode::Absolute), (Instruction::LSR, AddressMode::Absolute), (Instruction::SRE, AddressMode::Absolute),
    (Instruction::BVC, AddressMode::Relative), (Instruction::EOR, AddressMode::IndirectYIndex), (Instruction::JAM, AddressMode::Accumulator),   (Instruction::SRE, AddressMode::IndirectYIndex), (Instruction::NOP, AddressMode::ZeropageXIndex), (Instruction::EOR, AddressMode::ZeropageXIndex), (Instruction::LSR, AddressMode::ZeropageXIndex), (Instruction::SRE, AddressMode::ZeropageXIndex), (Instruction::CLI, AddressMode::Implied), (Instruction::EOR, AddressMode::AbsoluteYIndex), (Instruction::NOP, AddressMode::Implied), (Instruction::SRE, AddressMode::AbsoluteYIndex), (Instruction::NOP, AddressMode::AbsoluteXIndex), (Instruction::EOR, AddressMode::AbsoluteXIndex), (Instruction::LSR, AddressMode::AbsoluteXIndex), (Instruction::SRE, AddressMode::AbsoluteXIndex),
    (Instruction::RTS, AddressMode::Implied),  (Instruction::ADC, AddressMode::IndirectXIndex), (Instruction::JAM, AddressMode::Accumulator),   (Instruction::RRA, AddressMode::IndirectXIndex), (Instruction::NOP, AddressMode::Zeropage), (Instruction::ADC, AddressMode::Zeropage), (Instruction::ROR, AddressMode::Zeropage),                     (Instruction::RRA, AddressMode::Zeropage), (Instruction::PLA, AddressMode::Implied), (Instruction::ADC, AddressMode::Immediate), (Instruction::ROR, AddressMode::Accumulator), (Instruction::ARR, AddressMode::Immediate), (Instruction::JMP, AddressMode::Indirect), (Instruction::ADC, AddressMode::Absolute), (Instruction::ROR, AddressMode::Absolute), (Instruction::RRA, AddressMode::Absolute),
    (Instruction::BVS, AddressMode::Relative), (Instruction::ADC, AddressMode::IndirectYIndex), (Instruction::JAM, AddressMode::Accumulator),   (Instruction::RRA, AddressMode::IndirectYIndex), (Instruction::NOP, AddressMode::ZeropageXIndex), (Instruction::ADC, AddressMode::ZeropageXIndex), (Instruction::ROR, AddressMode::ZeropageXIndex), (Instruction::RRA, AddressMode::ZeropageXIndex), (Instruction::SEI, AddressMode::Implied), (Instruction::ADC, AddressMode::AbsoluteYIndex), (Instruction::NOP, AddressMode::Implied), (Instruction::RRA, AddressMode::AbsoluteYIndex), (Instruction::NOP, AddressMode::AbsoluteXIndex), (Instruction::ADC, AddressMode::AbsoluteXIndex), (Instruction::ROR, AddressMode::AbsoluteXIndex), (Instruction::RRA, AddressMode::AbsoluteXIndex),
    (Instruction::NOP, AddressMode::Immediate),(Instruction::STA, AddressMode::IndirectXIndex), (Instruction::NOP, AddressMode::Immediate),     (Instruction::SAX, AddressMode::IndirectXIndex), (Instruction::STY, AddressMode::Zeropage), (Instruction::STA, AddressMode::Zeropage), (Instruction::STX, AddressMode::Zeropage),                (Instruction::SAX, AddressMode::Zeropage), (Instruction::DEY, AddressMode::Implied), (Instruction::NOP, AddressMode::Immediate), (Instruction::TXA, AddressMode::Implied), (Instruction::ANE, AddressMode::Immediate), (Instruction::STY, AddressMode::Absolute), (Instruction::STA, AddressMode::Absolute), (Instruction::STX, AddressMode::Absolute), (Instruction::SAX, AddressMode::Absolute),
    (Instruction::BCC, AddressMode::Relative), (Instruction::STA, AddressMode::IndirectYIndex), (Instruction::JAM, AddressMode::Accumulator),   (Instruction::SHA, AddressMode::IndirectYIndex), (Instruction::STY, AddressMode::ZeropageXIndex), (Instruction::STA, AddressMode::ZeropageXIndex), (Instruction::STX, AddressMode::ZeropageYIndex), (Instruction::SAX, AddressMode::ZeropageYIndex), (Instruction::TYA, AddressMode::Implied), (Instruction::STA, AddressMode::AbsoluteYIndex), (Instruction::TXS, AddressMode::Implied), (Instruction::TAS, AddressMode::AbsoluteYIndex), (Instruction::SHY, AddressMode::AbsoluteXIndex), (Instruction::STA, AddressMode::AbsoluteXIndex), (Instruction::SHX, AddressMode::AbsoluteXIndex), (Instruction::SHA, AddressMode::AbsoluteYIndex),
    (Instruction::LDY, AddressMode::Immediate),(Instruction::LDA, AddressMode::IndirectXIndex), (Instruction::LDX, AddressMode::Immediate),     (Instruction::LAX, AddressMode::IndirectXIndex), (Instruction::LDY, AddressMode::Zeropage), (Instruction::LDA, AddressMode::Zeropage), (Instruction::LDX, AddressMode::Zeropage),                    (Instruction::LAX, AddressMode::Zeropage), (Instruction::TAY, AddressMode::Implied), (Instruction::LDA, AddressMode::Immediate), (Instruction::TAX, AddressMode::Implied), (Instruction::LXA, AddressMode::Immediate), (Instruction::LDY, AddressMode::Absolute), (Instruction::LDA, AddressMode::Absolute), (Instruction::LDX, AddressMode::Absolute), (Instruction::LAX, AddressMode::Absolute),
    (Instruction::BCS, AddressMode::Relative), (Instruction::LDA, AddressMode::IndirectYIndex), (Instruction::JAM, AddressMode::Accumulator),(Instruction::LAX, AddressMode::IndirectYIndex), (Instruction::LDY, AddressMode::ZeropageXIndex), (Instruction::LDA, AddressMode::ZeropageXIndex), (Instruction::LDX, AddressMode::ZeropageYIndex), (Instruction::LAX, AddressMode::ZeropageYIndex), (Instruction::CLV, AddressMode::Implied), (Instruction::LDA, AddressMode::AbsoluteYIndex), (Instruction::TSX, AddressMode::Implied), (Instruction::LAS, AddressMode::AbsoluteYIndex), (Instruction::LDY, AddressMode::AbsoluteXIndex), (Instruction::LDA, AddressMode::AbsoluteXIndex), (Instruction::LDX, AddressMode::AbsoluteYIndex), (Instruction::LAX, AddressMode::AbsoluteYIndex),
    (Instruction::CPY, AddressMode::Immediate),(Instruction::CMP, AddressMode::IndirectXIndex), (Instruction::NOP, AddressMode::Immediate),       (Instruction::DCP, AddressMode::IndirectXIndex), (Instruction::CPY, AddressMode::Zeropage), (Instruction::CMP, AddressMode::Zeropage), (Instruction::DEC, AddressMode::Zeropage),                    (Instruction::DCP, AddressMode::Zeropage), (Instruction::INY, AddressMode::Implied), (Instruction::CMP, AddressMode::Immediate), (Instruction::DEX, AddressMode::Implied), (Instruction::SBX, AddressMode::Immediate), (Instruction::CPY, AddressMode::Absolute), (Instruction::CMP, AddressMode::Absolute), (Instruction::DEC, AddressMode::Absolute), (Instruction::DCP, AddressMode::Absolute),
    (Instruction::BNE, AddressMode::Relative), (Instruction::CMP, AddressMode::IndirectYIndex), (Instruction::JAM, AddressMode::Accumulator),   (Instruction::DCP, AddressMode::IndirectYIndex), (Instruction::NOP, AddressMode::ZeropageXIndex), (Instruction::CMP, AddressMode::ZeropageXIndex), (Instruction::DEC, AddressMode::ZeropageXIndex), (Instruction::DCP, AddressMode::ZeropageXIndex), (Instruction::CLD, AddressMode::Implied), (Instruction::CMP, AddressMode::AbsoluteYIndex), (Instruction::NOP, AddressMode::Implied), (Instruction::DCP, AddressMode::AbsoluteXIndex), (Instruction::NOP, AddressMode::AbsoluteXIndex), (Instruction::CMP, AddressMode::AbsoluteXIndex), (Instruction::DEC, AddressMode::AbsoluteXIndex), (Instruction::DCP, AddressMode::AbsoluteXIndex),
    (Instruction::CPX, AddressMode::Immediate),(Instruction::SBC, AddressMode::IndirectXIndex), (Instruction::NOP, AddressMode::Immediate),       (Instruction::ISC, AddressMode::IndirectXIndex), (Instruction::CPX, AddressMode::Zeropage), (Instruction::SBC, AddressMode::Zeropage), (Instruction::INC, AddressMode::Zeropage),                    (Instruction::ISC, AddressMode::Zeropage), (Instruction::INX, AddressMode::Implied), (Instruction::SBC, AddressMode::Immediate), (Instruction::NOP, AddressMode::Implied), (Instruction::USBC, AddressMode::Immediate), (Instruction::CPX, AddressMode::Absolute), (Instruction::SBC, AddressMode::Absolute), (Instruction::INC, AddressMode::Absolute), (Instruction::ISC, AddressMode::Absolute),
    (Instruction::BEQ, AddressMode::Relative), (Instruction::SBC, AddressMode::IndirectYIndex), (Instruction::JAM, AddressMode::Accumulator),   (Instruction::ISC, AddressMode::IndirectYIndex), (Instruction::NOP, AddressMode::ZeropageXIndex), (Instruction::SBC, AddressMode::ZeropageXIndex), (Instruction::INC, AddressMode::ZeropageXIndex), (Instruction::ISC, AddressMode::ZeropageXIndex), (Instruction::SED, AddressMode::Implied), (Instruction::SBC, AddressMode::AbsoluteYIndex), (Instruction::NOP, AddressMode::Implied), (Instruction::ISC, AddressMode::AbsoluteXIndex), (Instruction::NOP, AddressMode::AbsoluteXIndex), (Instruction::SBC, AddressMode::AbsoluteXIndex), (Instruction::INC, AddressMode::AbsoluteXIndex), (Instruction::ISC, AddressMode::AbsoluteXIndex)
];

const CYCLE_TIMES: [(u8, CycleAddition); 256] = [
    (7, CycleAddition::None), (6, CycleAddition::None), (0, CycleAddition::None), (8, CycleAddition::None), (3, CycleAddition::None), (3, CycleAddition::None), (5, CycleAddition::None), (5, CycleAddition::None), (3, CycleAddition::None), (2, CycleAddition::None), (2, CycleAddition::None), (2, CycleAddition::None), (4, CycleAddition::None), (4, CycleAddition::None), (6, CycleAddition::None), (6, CycleAddition::None), 
    (2, CycleAddition::BranchOnPage), (5, CycleAddition::PageBoundaryCrossed), (0, CycleAddition::None), (8, CycleAddition::None), (4, CycleAddition::None), (4, CycleAddition::None), (6, CycleAddition::None), (6, CycleAddition::None), (2, CycleAddition::None), (4, CycleAddition::PageBoundaryCrossed), (2, CycleAddition::None), (7, CycleAddition::None), (4, CycleAddition::PageBoundaryCrossed), (4, CycleAddition::PageBoundaryCrossed), (7, CycleAddition::None), (7, CycleAddition::None), 
    (6, CycleAddition::None), (6, CycleAddition::None), (0, CycleAddition::None), (8, CycleAddition::None), (3, CycleAddition::None), (3, CycleAddition::None), (5, CycleAddition::None), (5, CycleAddition::None), (4, CycleAddition::None), (2, CycleAddition::None), (2, CycleAddition::None), (2, CycleAddition::None), (4, CycleAddition::None), (4, CycleAddition::None), (6, CycleAddition::None), (6, CycleAddition::None), 
    (2, CycleAddition::BranchOnPage), (5, CycleAddition::PageBoundaryCrossed), (0, CycleAddition::None), (8, CycleAddition::None), (4, CycleAddition::None), (4, CycleAddition::None), (6, CycleAddition::None), (6, CycleAddition::None), (2, CycleAddition::None), (4, CycleAddition::PageBoundaryCrossed), (2, CycleAddition::None), (7, CycleAddition::None), (4, CycleAddition::PageBoundaryCrossed), (4, CycleAddition::PageBoundaryCrossed), (7, CycleAddition::None), (7, CycleAddition::None), 
    (6, CycleAddition::None), (6, CycleAddition::None), (0, CycleAddition::None), (8, CycleAddition::None), (3, CycleAddition::None), (3, CycleAddition::None), (5, CycleAddition::None), (5, CycleAddition::None), (3, CycleAddition::None), (2, CycleAddition::None), (2, CycleAddition::None), (2, CycleAddition::None), (3, CycleAddition::None), (4, CycleAddition::None), (6, CycleAddition::None), (6, CycleAddition::None), 
    (2, CycleAddition::BranchOnPage), (5, CycleAddition::PageBoundaryCrossed), (0, CycleAddition::None), (8, CycleAddition::None), (4, CycleAddition::None), (4, CycleAddition::None), (6, CycleAddition::None), (6, CycleAddition::None), (2, CycleAddition::None), (4, CycleAddition::PageBoundaryCrossed), (2, CycleAddition::None), (7, CycleAddition::None), (4, CycleAddition::PageBoundaryCrossed), (4, CycleAddition::PageBoundaryCrossed), (7, CycleAddition::None), (7, CycleAddition::None), 
    (6, CycleAddition::None), (6, CycleAddition::None), (0, CycleAddition::None), (8, CycleAddition::None), (3, CycleAddition::None), (3, CycleAddition::None), (5, CycleAddition::None), (5, CycleAddition::None), (4, CycleAddition::None), (2, CycleAddition::None), (2, CycleAddition::None), (2, CycleAddition::None), (5, CycleAddition::None), (4, CycleAddition::None), (6, CycleAddition::None), (6, CycleAddition::None), 
    (2, CycleAddition::BranchOnPage), (5, CycleAddition::PageBoundaryCrossed), (0, CycleAddition::None), (8, CycleAddition::None), (4, CycleAddition::None), (4, CycleAddition::None), (6, CycleAddition::None), (6, CycleAddition::None), (2, CycleAddition::None), (4, CycleAddition::PageBoundaryCrossed), (2, CycleAddition::None), (7, CycleAddition::None), (4, CycleAddition::PageBoundaryCrossed), (4, CycleAddition::PageBoundaryCrossed), (7, CycleAddition::None), (7, CycleAddition::None), 
    (2, CycleAddition::None), (6, CycleAddition::None), (2, CycleAddition::None), (6, CycleAddition::None), (3, CycleAddition::None), (3, CycleAddition::None), (3, CycleAddition::None), (3, CycleAddition::None), (2, CycleAddition::None), (2, CycleAddition::None), (2, CycleAddition::None), (2, CycleAddition::None), (4, CycleAddition::None), (4, CycleAddition::None), (4, CycleAddition::None), (4, CycleAddition::None), 
    (2, CycleAddition::BranchOnPage), (6, CycleAddition::None), (0, CycleAddition::None), (6, CycleAddition::None), (4, CycleAddition::None), (4, CycleAddition::None), (4, CycleAddition::None), (4, CycleAddition::None), (2, CycleAddition::None), (5, CycleAddition::None), (2, CycleAddition::None), (5, CycleAddition::None), (5, CycleAddition::None), (5, CycleAddition::None), (5, CycleAddition::None), (5, CycleAddition::None), 
    (2, CycleAddition::None), (6, CycleAddition::None), (2, CycleAddition::None), (6, CycleAddition::None), (3, CycleAddition::None), (3, CycleAddition::None), (3, CycleAddition::None), (3, CycleAddition::None), (2, CycleAddition::None), (2, CycleAddition::None), (2, CycleAddition::None), (2, CycleAddition::None), (4, CycleAddition::None), (4, CycleAddition::None), (4, CycleAddition::None), (4, CycleAddition::None), 
    (2, CycleAddition::BranchOnPage), (5, CycleAddition::PageBoundaryCrossed), (0, CycleAddition::None), (5, CycleAddition::PageBoundaryCrossed), (4, CycleAddition::None), (4, CycleAddition::None), (4, CycleAddition::None), (4, CycleAddition::None), (2, CycleAddition::None), (4, CycleAddition::PageBoundaryCrossed), (2, CycleAddition::None), (4, CycleAddition::PageBoundaryCrossed), (4, CycleAddition::PageBoundaryCrossed), (4, CycleAddition::PageBoundaryCrossed), (4, CycleAddition::PageBoundaryCrossed), (4, CycleAddition::PageBoundaryCrossed), 
    (2, CycleAddition::None), (6, CycleAddition::None), (2, CycleAddition::None), (8, CycleAddition::None), (3, CycleAddition::None), (3, CycleAddition::None), (5, CycleAddition::None), (5, CycleAddition::None), (2, CycleAddition::None), (2, CycleAddition::None), (2, CycleAddition::None), (2, CycleAddition::None), (4, CycleAddition::None), (4, CycleAddition::None), (6, CycleAddition::None), (6, CycleAddition::None), 
    (2, CycleAddition::BranchOnPage), (5, CycleAddition::PageBoundaryCrossed), (0, CycleAddition::None), (8, CycleAddition::None), (4, CycleAddition::None), (4, CycleAddition::None), (6, CycleAddition::None), (6, CycleAddition::None), (2, CycleAddition::None), (4, CycleAddition::PageBoundaryCrossed), (2, CycleAddition::None), (7, CycleAddition::None), (4, CycleAddition::PageBoundaryCrossed), (4, CycleAddition::PageBoundaryCrossed), (7, CycleAddition::None), (7, CycleAddition::None), 
    (2, CycleAddition::None), (6, CycleAddition::None), (2, CycleAddition::None), (8, CycleAddition::None), (3, CycleAddition::None), (3, CycleAddition::None), (5, CycleAddition::None), (5, CycleAddition::None), (2, CycleAddition::None), (2, CycleAddition::None), (2, CycleAddition::None), (2, CycleAddition::None), (4, CycleAddition::None), (4, CycleAddition::None), (6, CycleAddition::None), (6, CycleAddition::None), 
    (2, CycleAddition::BranchOnPage), (5, CycleAddition::PageBoundaryCrossed), (0, CycleAddition::None), (8, CycleAddition::None), (4, CycleAddition::None), (4, CycleAddition::None), (6, CycleAddition::None), (6, CycleAddition::None), (2, CycleAddition::None), (4, CycleAddition::PageBoundaryCrossed), (2, CycleAddition::None), (7, CycleAddition::None), (4, CycleAddition::PageBoundaryCrossed), (4, CycleAddition::PageBoundaryCrossed), (7, CycleAddition::None), (7, CycleAddition::None)
];

#[derive(Debug, Clone, Copy)]
enum CycleAddition {
    None,
    PageBoundaryCrossed,
    BranchOnPage
}

#[derive(Debug, Clone, Copy)]
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
    TYA,
    
    // VVVV Illegal

    /// AND oper + LSR
    ALR,
    /// AND oper + set C asl 7
    ANC,
    /// AND X + AND oper
    ANE,
    /// AND oper + ROR
    ARR,
    /// DEC oper + CMP oper
    DCP,
    /// INC oper + SBC oper
    ISC,
    /// LDA/TSX oper
    LAS,
    /// LDA oper + LDX oper
    LAX,
    /// Store * AND oper in A and X
    LXA,
    /// ROL oper + AND oper
    RLA,
    /// ROR oper + ADC oper
    RRA,
    /// A AND X -> M (what the fuck)
    SAX,
    /// (A AND X) - oper -> X
    SBX,
    /// A AND X AND (H+1) -> M (what the fuck)
    SHA,
    /// X AND (H+1) -> M
    SHX,
    /// Y AND (H+1) -> M
    SHY,
    /// ASL oper + ORA oper
    SLO,
    /// LSR oper + EOR oper
    SRE,
    /// A AND X -> SP, A AND X AND (H+1) -> M
    TAS,
    /// SBC oper + NOP (what the fuck)
    USBC,
    /// Jams.
    JAM,
    Illegal
}

#[derive(Debug, Copy, Clone)]
pub enum AddressMode {
    Accumulator,
    Absolute,
    AbsoluteXIndex,
    AbsoluteYIndex,
    Immediate,
    Implied,
    Indirect,
    IndirectXIndex,
    IndirectYIndex,
    Relative,
    Zeropage,
    ZeropageXIndex,
    ZeropageYIndex,
    Illegal
}

#[derive(Debug, Clone, Copy)]
pub struct StatusRegister {
    pub negative: bool,
    pub overflow: bool,
    pub ignored: bool,
    pub _break: bool,
    pub decimal: bool,
    pub interrupt: bool,
    pub zero: bool,
    pub carry: bool
}

impl StatusRegister {
    pub fn new() -> Self {
        StatusRegister { negative: false, overflow: false, ignored: false, _break: false, decimal: false, interrupt: false, zero: false, carry: false }
    }

    pub fn to_u8(&self) -> u8 {
        (self.negative as u8) << 7 |
        (self.overflow as u8) << 6 |
        (self.ignored as u8) << 5 |
        (self._break as u8) << 4 |
        (self.decimal as u8) << 3 |
        (self.interrupt as u8) << 2 |
        (self.zero as u8) << 1 |
        (self.carry as u8)
    }

    pub fn from_u8(x: u8) -> Self {
       StatusRegister {
           negative: (x & 0b10000000) >> 7 == 1,
           overflow: (x & 0b01000000) >> 6 == 1,
           ignored: (x & 0b00100000) >> 5 == 1,
           _break: (x & 0b00010000) >> 4== 1,
           decimal: (x & 0b00001000) >> 3 == 1,
           interrupt: (x & 0b00000100) >> 2 == 1,
           zero: (x & 0b00000010) >> 1 == 1,
           carry: (x & 0b00000001) == 1
       }
    }
}

#[cfg(test)]
mod status_register_test {
    use super::StatusRegister;

    #[test]
    pub fn test() {
        let status = StatusRegister::from_u8(106);
        assert_eq!(status.to_u8(), 106);
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CpuConfig {
    pub decimal_mode: bool
}
pub struct Cpu<T: bus::CPUMemory> {
    pub previous_program_counter: u16,
    pub program_counter: u16,
    pub accumulator: u8,
    pub x: u8,
    pub y: u8,
    pub status: StatusRegister,
    pub stack_pointer: u8,
    pub bus: T,
    pub config: CpuConfig
}

impl<T: bus::CPUMemory> Display for Cpu<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:5}| {:6} | {:18} | {:6}\n", "REG", "HEX", "BIN", "DEC")?;
        write!(f, "{:-<43}\n", "")?;
        write!(f, "{:5}| {:#6X} | {:#18b} | {:6}\n", "PC", self.program_counter, self.program_counter, self.program_counter)?;
        write!(f, "{:5}| {:#6X} | {:#18b} | {:6}\n", "ACC", self.accumulator, self.accumulator, self.accumulator)?;
        write!(f, "{:5}| {:#6X} | {:#18b} | {:6}\n", "X", self.x, self.x, self.x)?;
        write!(f, "{:5}| {:#6X} | {:#18b} | {:6}\n", "Y", self.y, self.y, self.y)?;
        write!(f, "{:5}| {:#6X} | {:#18b} | {:6}\n", "STAT", self.status.to_u8(), self.status.to_u8(), self.status.to_u8())?;
        write!(f, "{:5}| {:#6X} | {:#18b} | {:6}\n", "SP", self.stack_pointer, self.stack_pointer, self.stack_pointer) 
    }
}

impl<T: bus::CPUMemory> Cpu<T> {
    pub fn new(bus: T, config: CpuConfig, status: StatusRegister) -> Self {
        Cpu {
            previous_program_counter: 0x0,
            program_counter: 0x400,
            accumulator: 0,
            x: 0,
            y: 0,
            status,
            stack_pointer: 0xFD,
            bus,
            config
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
        self.status.negative = (operand as i8) < 0;
    }

    /// (Operand, Address, PageBoundaryCrossed)
    /// PageBoundaryCrossed will only possibly be true in AbsoluteXIndex, AbsoluteYIndex, and IndirectYIndex
    pub fn fetch_operand(&mut self, address_mode: AddressMode) -> (u8, u16, bool) {
        match address_mode {
            AddressMode::Accumulator => {
                (self.accumulator, 0, false)
            },
            AddressMode::Absolute => {
                let low_byte = self.bus.read(self.program_counter.wrapping_add(1));
                let high_byte = self.bus.read(self.program_counter.wrapping_add(2));
                self.program_counter = self.program_counter.wrapping_add(2);
                let address = ((high_byte as u16) << 8) | low_byte as u16;
                (self.bus.read(address), address, false)
            },
            AddressMode::AbsoluteXIndex => {
                let low_byte = self.bus.read(self.program_counter.wrapping_add(1));
                let high_byte = self.bus.read(self.program_counter.wrapping_add(2));
                self.program_counter = self.program_counter.wrapping_add(2);
                let address = ((high_byte as u16) << 8) | low_byte as u16;
                let address_indexed = address.wrapping_add(self.x as u16);
                (self.bus.read(address_indexed), address_indexed, address_indexed & 0xFF00 != address & 0xFF00)
            },
            AddressMode::AbsoluteYIndex => {
                let low_byte = self.bus.read(self.program_counter.wrapping_add(1));
                let high_byte = self.bus.read(self.program_counter.wrapping_add(2));
                self.program_counter = self.program_counter.wrapping_add(2);
                let address = ((high_byte as u16) << 8) | low_byte as u16;
                let address_indexed = address.wrapping_add(self.y as u16);
                (self.bus.read(address_indexed), address_indexed, address_indexed & 0xFF00 != address & 0xFF00)
            },
            AddressMode::Immediate => {
                let byte = self.bus.read(self.program_counter.wrapping_add(1));
                self.program_counter = self.program_counter.wrapping_add(1);
                (byte, self.program_counter, false)
            },
            AddressMode::Implied => {
                (0, 0, false)
            },
            AddressMode::Indirect => {
                let low_byte = self.bus.read(self.program_counter + 1);
                let high_byte = self.bus.read(self.program_counter + 2);
                self.program_counter = self.program_counter.wrapping_add(2);
                let address = (u16::from(high_byte) << 8) | u16::from(low_byte);
                // page boundary bug
                if address & 0xFF == 0xFF {
                    let low_byte = self.bus.read(address);
                    let high_byte = self.bus.read(address & 0xFF00);
                    let indirect = (u16::from(high_byte) << 8) | u16::from(low_byte);
                    (0,indirect, false)
                } else {
                    let low_byte = self.bus.read(address);
                    let high_byte = self.bus.read(address.wrapping_add(1));
                    let indirect = (u16::from(high_byte) << 8) | u16::from(low_byte);
                    (0,indirect, false)
                }
                
            },
            AddressMode::IndirectXIndex => {
                let byte = self.bus.read(self.program_counter.wrapping_add(1));
                self.program_counter = self.program_counter.wrapping_add(1);
                let low_byte = self.bus.read((byte.wrapping_add(self.x)) as u16);
                let high_byte = self.bus.read(byte.wrapping_add(self.x).wrapping_add(1) as u16);
                let address = ((high_byte as u16) << 8) | low_byte as u16;

                (self.bus.read(address), address, false)
            },
            AddressMode::IndirectYIndex => {
                // fetch  address at zero page
                let zeropage_byte = self.bus.read(self.program_counter.wrapping_add(1));
                self.program_counter = self.program_counter.wrapping_add(1);
                let low_byte = self.bus.read((zeropage_byte) as u16);
                let high_byte = self.bus.read((zeropage_byte.wrapping_add(1)) as u16);
                let address = ((high_byte as u16) << 8) | low_byte as u16;
                let address_indexed = address.wrapping_add(self.y as u16);
                (self.bus.read(address_indexed), address_indexed, address_indexed & 0xFF00 != address & 0xFF00)
            },
            AddressMode::Relative => {
                // this will be broken
                let byte = self.bus.read(self.program_counter.wrapping_add(1));
                let byte = byte as i8;
                self.program_counter = self.program_counter.wrapping_add(1);
                let address = (self.program_counter as i16).wrapping_add(byte as i16);
                (0,address as u16, false)
            },
            AddressMode::Zeropage => {
                let low_byte = self.bus.read(self.program_counter.wrapping_add(1));
                self.program_counter = self.program_counter.wrapping_add(1);
                (self.bus.read(low_byte as u16), low_byte as u16, false)
            },
            AddressMode::ZeropageXIndex => {
                let low_byte = self.bus.read(self.program_counter.wrapping_add(1));
                self.program_counter = self.program_counter.wrapping_add(1);
                (self.bus.read(self.x.wrapping_add(low_byte) as u16), self.x.wrapping_add(low_byte) as u16, false)
            },
            AddressMode::ZeropageYIndex => {
                let low_byte = self.bus.read(self.program_counter.wrapping_add(1));
                self.program_counter = self.program_counter.wrapping_add(1);
                (self.bus.read(self.y.wrapping_add(low_byte) as u16), self.y.wrapping_add(low_byte) as u16, false)
            },
            AddressMode::Illegal => {
                panic!();
            }
        }

    }

    pub fn reset(&mut self) {
        let high_byte = self.bus.read(0xFFFD);
        let low_byte = self.bus.read(0xFFFC);

        let new_address = (high_byte as u16) << 8 & (low_byte as u16);
        self.program_counter = new_address;
    }

    pub fn step(&mut self) -> usize {
        let opcode = self.bus.read(self.program_counter);

        let high_nibble = (opcode & 0xF0) >> 4;
        let low_nibble = opcode & 0x0F;

        let (opcode, address_mode) = LOOKUP_TABLE[(high_nibble * 16 + low_nibble) as usize];
        let (operand, address, page_boundary_crossed) = self.fetch_operand(address_mode);

        match opcode {
            Instruction::ADC => self.adc(operand),
            Instruction::AND => self.and(operand),
            Instruction::ASL => match address_mode {
                AddressMode::Accumulator => self.asl(address, operand, true),
                _ => self.asl(address, operand, false)
            },
            Instruction::BCC => self.bcc(address),
            Instruction::BCS => self.bcs(address),
            Instruction::BEQ => self.beq(address),
            Instruction::BIT => self.bit(operand),
            Instruction::BMI => self.bmi(address),
            Instruction::BNE => self.bne(address),
            Instruction::BPL => self.bpl(address),
            Instruction::BRK => self.brk(),
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
            Instruction::LSR => match address_mode {
                AddressMode::Accumulator => self.lsr(address, operand, true),
                _ => self.lsr(address, operand, false)
            },
            Instruction::NOP => self.nop(),
            Instruction::ORA => self.ora(operand),
            Instruction::PHA => self.pha(),
            Instruction::PHP => self.php(),
            Instruction::PLA => self.pla(),
            Instruction::PLP => self.plp(),
            Instruction::ROL => match address_mode {
                AddressMode::Accumulator => self.rol(address, operand, true),
                _ => self.rol(address, operand, false)
            },
            Instruction::ROR => match address_mode {
                AddressMode::Accumulator => self.ror(address, operand, true),
                _ => self.ror(address, operand, false)
            },
            Instruction::RTS => self.rts(),
            Instruction::RTI => self.rti(),
            Instruction::SBC => self.sbc(operand),
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
            // VVVV Illegal
            Instruction::ALR => self.alr(operand),
            Instruction::ANC => self.anc(operand),
            Instruction::ANE => self.ane(operand),
            Instruction::ARR => self.arr(address, operand),
            Instruction::DCP => self.dcp(address, operand),
            Instruction::ISC => self.isc(address, operand),
            Instruction::LAS => self.las(operand),
            Instruction::LAX => self.lax(operand),
            Instruction::LXA => self.lxa(operand),
            Instruction::RLA => self.rla(address, operand),
            Instruction::RRA => self.rra(address, operand),
            Instruction::SAX => self.sax(address),
            Instruction::SBX => self.sbx(operand),
            Instruction::SHA => self.sha(address),
            Instruction::SHX => self.shx(address),
            Instruction::SHY => self.shy(address),
            Instruction::SLO => self.slo(address, operand),
            Instruction::SRE => self.sre(address, operand),
            Instruction::TAS => self.tas(address),
            Instruction::USBC => self.usbc(operand),
            // TODO: Implement jam functionality? 
            Instruction::JAM => {
                return 0;
            },
            Instruction::Illegal => panic!(),
        }

        self.previous_program_counter = self.program_counter;
        self.program_counter = self.program_counter.wrapping_add(1);

        let (cycles, addition) = CYCLE_TIMES[(high_nibble * 16 + low_nibble) as usize];
        match addition {
            CycleAddition::None => cycles as usize,
            CycleAddition::PageBoundaryCrossed => {
                if page_boundary_crossed {
                    cycles as usize + 1
                } else {
                    cycles as usize
                }
            },
            CycleAddition::BranchOnPage => {
                if self.previous_program_counter & 0xFF00 == self.program_counter & 0xFF00 {
                    cycles as usize + 1
                } else {
                    cycles as usize + 2
                }
            },
        }

    }

    pub fn push_to_stack(&mut self, x: u8) {
        self.bus.write(self.stack_pointer as u16 + 0x100, x);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    pub fn pop_from_stack(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        let x = self.bus.read(self.stack_pointer as u16 + 0x100);
        x
    }

    pub fn compare(&mut self, r: i8, val: i8) {
        if r as u8 >= val as u8 {
            self.status.carry = true;
        } else {
            self.status.carry = false;
        }

        if r as i8 == val as i8 {
            self.status.zero = true;
        } else {
            self.status.zero = false;
        }

        let diff: i8 = r.wrapping_sub(val as i8);
        if diff < 0 {
            self.status.negative = true;
        } else {
            self.status.negative = false;
        }
    }

    pub fn lower_digit(&mut self, x: u8) -> u8 {
        x | 0xF
    }

    pub fn upper_digit(&mut self, x: u8) -> u8 {
        (x | 0xF0) >> 4
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
    }

    pub fn php(&mut self) {
        self.push_to_stack(self.status.to_u8() | (0b00110000));
    }

    pub fn pla(&mut self) {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        let new_accumulator = self.bus.read(self.stack_pointer as u16 + 0x100);
        self.accumulator = new_accumulator;
        self.set_if_negative(new_accumulator);
        self.set_if_zero(new_accumulator);
    }


    pub fn plp(&mut self) {
        let stack_status = self.pop_from_stack();
        let current_status = self.status.to_u8();
        
        let new_status = (stack_status & 0b11001111) | (current_status & 0b00110000);
        self.status = StatusRegister::from_u8(new_status);
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
        let mut result: u16 = u16::from(self.accumulator) + u16::from(operand) + u16::from(self.status.carry);
        if self.status.decimal && self.config.decimal_mode {
            let mut sum = (self.accumulator & 0xF) + (operand & 0xF) + u8::from(self.status.carry);
            if sum >= 0xA {
                sum = ((sum + 0x6) & 0xF) + 0x10;
            }
            let mut sum = u16::from(self.accumulator & 0xF0) + u16::from(operand & 0xF0) + u16::from(sum);
            self.status.zero = result & 0xFF == 0;
            self.status.negative = (sum & 0x80) > 0;
            self.status.overflow = (!(self.accumulator ^ operand) & (self.accumulator ^ sum as u8) & 0x80) > 0;
            if sum >= 0xA0 {
                sum += 0x60;
            }
            self.status.carry = sum >= 0x100;
            result = sum & 0xFF;
        } else {
            self.status.carry = result > u16::from(u8::max_value());
            self.status.zero = result as u8 == 0;
            self.status.overflow = (!(self.accumulator ^ operand) & (self.accumulator ^ result as u8) & 0x80) > 0;
            self.status.negative = (result as u8 & 0x80) > 0;
        }
        self.accumulator = result as u8;
    }

    pub fn sbc(&mut self, operand: u8) {
        let mut result = u16::from(self.accumulator) + u16::from(!operand) + (if self.status.carry { 1 } else { 0 });
        self.status.carry = result > u16::from(u8::max_value());
        self.status.zero = result as u8 == 0;
        self.status.overflow = ((self.accumulator ^ operand) & (self.accumulator ^ result as u8) & 0x80) > 0;
        self.status.negative = (result as u8 & 0x80) > 0;
        if self.status.decimal && self.status.decimal {
            let operand = operand as i16;
            let mut sum = (self.accumulator & 0xF) as i16 - (operand & 0xF) + self.status.carry as i16 - 1;
            if sum < 0 {
                sum = ((sum - 0x6) & 0xF) - 0x10;
            }
            let mut sum = (self.accumulator & 0xF0) as i16 - (operand & 0xF0) + sum;
            if sum < 0 {
                sum -= 0x60;
            }
            result = (sum & 0xFF) as u16;
        }
        self.accumulator = result as u8;
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
            self.status.carry = self.accumulator >> 7 == 1;
            self.accumulator = self.accumulator << 1;
            self.status.negative = self.accumulator & 0x80 > 0;
            self.status.zero = self.accumulator == 0;
        } else {
            self.status.carry = operand >> 7 == 1;
            let new_num = operand << 1;
            self.bus.write(address, new_num);
            self.status.negative = new_num & 0x80 > 0;
            self.status.zero = new_num == 0;
        }
    }

    pub fn lsr(&mut self, address: u16, operand: u8, accumulator: bool) {
        if accumulator {
            self.status.carry = self.accumulator & 0b1 == 1;
            self.accumulator = self.accumulator >> 1;
            self.set_if_negative(self.accumulator);
            self.set_if_zero(self.accumulator);
        } else {
            self.status.carry = operand & 0b1 == 1;
            let new_num = operand >> 1;
            self.bus.write(address, new_num);
            self.set_if_negative(new_num);
            self.set_if_zero(new_num);
        }
    }

    pub fn rol(&mut self, address: u16, operand: u8, accumulator: bool) {
        if accumulator {
            let carry = self.status.carry;
            self.status.carry = (self.accumulator >> 7) == 1;
            self.accumulator = (self.accumulator << 1) + carry as u8;
            self.status.negative = self.accumulator & 0x80 > 0;
            self.status.zero = self.accumulator == 0;
        } else {
            let carry = self.status.carry;
            self.status.carry = (operand >> 7) == 1;
            let new = (operand << 1) + carry as u8;
            self.bus.write(address, new);
            self.status.negative = new & 0x80 > 0;
            self.status.zero = new == 0;
        }
    }

    pub fn ror(&mut self, address: u16, operand: u8, accumulator: bool) {
        if accumulator {
            let carry = self.status.carry as u8;
            self.status.carry = (self.accumulator & 1) == 1;
            self.accumulator = (self.accumulator >> 1) + (carry << 7);
            self.status.negative = self.accumulator & 0x80 > 0;
            self.status.zero = self.accumulator == 0;
        } else {
            let carry = self.status.carry as u8;
            self.status.carry = (operand & 1) == 1;
            let new = (operand >> 1) + (carry << 7);
            self.bus.write(address, new);
            self.status.negative = new & 0x80 > 0;
            self.status.zero = new == 0;
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

    // Comparisons - stolen from another rust emulator
    pub fn cmp(&mut self, operand: u8) {
        self.compare(self.accumulator as i8, operand as i8);
    }

    pub fn cpx(&mut self, operand: u8) {
        self.compare(self.x as i8, operand as i8);
    }

    pub fn cpy(&mut self, operand: u8) {
        self.compare(self.y as i8, operand as i8);
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
        // step is always incrementing, so if we dont decrement address will be one off what it should be.
        self.program_counter = address.wrapping_sub(1);
    }

    pub fn jsr(&mut self, address: u16) {
        let high_byte = ((self.program_counter) >> 8) as u8;
        let low_byte = ((self.program_counter) & 0b11111111) as u8;
        self.push_to_stack(high_byte);
        self.push_to_stack(low_byte);

        self.program_counter = address - 1; // step
    }

    pub fn rts(&mut self) {
        let low_byte = self.pop_from_stack();
        let high_byte = self.pop_from_stack();
        self.program_counter = ((high_byte as u16) << 8) | (low_byte as u16);
    }

    // Interrupts
    pub fn brk(&mut self) {
        self.program_counter += 2;
        let high_byte = (self.program_counter >> 8) as u8;
        let low_byte = (self.program_counter & 0xFF) as u8;
        self.push_to_stack(high_byte);
        self.push_to_stack(low_byte);
        self.status._break = true;
        self.push_to_stack(self.status.to_u8());
        self.status._break = false;
        self.status.interrupt = true;

        let new_high_byte = self.bus.read(0xFFFF);
        let new_low_byte = self.bus.read(0xFFFE);
        let pc = ((new_high_byte as u16) << 8) | new_low_byte as u16;
        self.program_counter = pc - 1; // cause of step() always incrementing
    }

    pub fn rti(&mut self) {
        let mut status = StatusRegister::from_u8(self.pop_from_stack());
        status.ignored = true;
        status._break = false;
        let low_byte = self.pop_from_stack();
        let high_byte = self.pop_from_stack();
        let program_counter = ((high_byte as u16) << 8) | low_byte as u16;

        self.status = status;
        self.program_counter = program_counter - 1;
    }

    // Other
    pub fn bit(&mut self, operand: u8) {
        self.status.negative = (operand & 0b10000000) > 0;
        self.status.overflow = (operand & 0b01000000) > 0;

        self.status.zero = (operand & self.accumulator) == 0;
    }

    pub fn nop(&mut self) {
        // its noping time!
    }

    // Illegal Instructions!
    pub fn alr(&mut self, operand: u8) {
        self.and(operand);
        self.lsr(0x0, operand, true);
    }

    pub fn anc(&mut self, operand: u8) {
        self.and(operand);
        self.status.carry = (self.accumulator & 0x80) > 0;
    }

    pub fn ane(&mut self, operand: u8) {
        self.accumulator = (self.accumulator | 0xFF) & self.x & operand;
        self.status.negative = self.accumulator & 0x80 > 0;
        self.status.zero = self.accumulator == 0;
    }

    // FIXME: Problem child! WHERE DOES BIT 7 come from?
    pub fn arr(&mut self, address: u16, operand: u8) {
        let bit7_1 = (operand & 0x80) > 0;
        self.and(operand);
        let bit7_2 = (self.accumulator& 0x80) > 0;
        self.ror(0x0, self.bus.read(address), true);
        let bit7_3 = (self.bus.read(address) & 0x80) > 0;
        let bit7_4 = (self.accumulator & 0x80) > 0;
        self.status.carry = bit7_4;
    }

    pub fn dcp(&mut self, address: u16, operand: u8) {
        self.dec(address, operand);
        self.cmp(self.bus.read(address));
    }

    pub fn isc(&mut self, address: u16, operand: u8) {
        self.inc(address, operand);
        self.sbc(self.bus.read(address));
    }

    pub fn las(&mut self, operand: u8) {
        let result = operand & self.stack_pointer;
        self.accumulator = result;
        self.x = result;
        self.stack_pointer = result;

        self.status.negative = result & 0x80 > 0;
        self.status.zero = result == 0;
    }

    pub fn lax(&mut self, operand: u8) {
        self.lda(operand);
        self.ldx(self.accumulator);
    }

    pub fn lxa(&mut self, operand: u8) {
        let result = (self.accumulator | 0xFF) & operand;
        self.accumulator = result;
        self.x = self.accumulator;

        self.status.negative = result & 0x80 > 0;
        self.status.zero = result == 0;
    }

    pub fn rla(&mut self, address: u16, operand: u8) {
        self.rol(address, operand, false);
        self.and(self.bus.read(address));
    }

    pub fn rra(&mut self, address: u16, operand: u8) {
        self.ror(address, operand, false);
        self.adc(self.bus.read(address));
    }

    pub fn sax(&mut self, address: u16) {
        self.bus.write(address, self.accumulator & self.x);
    }

    pub fn sbx(&mut self, operand: u8) {
        self.x = (self.accumulator & self.x).wrapping_sub(operand);
        self.status.negative = self.x & 0x80 > 0;
        self.status.zero = self.x == 0;
    }

    pub fn sha(&mut self, address: u16) {
        let high_byte = (((address & 0xFF00) >> 8) + 1) as u8;
        self.bus.write(address, self.accumulator & self.x & high_byte);
    }   

    pub fn shx(&mut self, address: u16) {
        let high_byte = (((address & 0xFF00) >> 8) + 1) as u8;
        self.bus.write(address, self.x & high_byte);
    }

    pub fn shy(&mut self, address: u16) {
        let high_byte = (((address & 0xFF00) >> 8) + 1) as u8;
        self.bus.write(address, self.y & high_byte);    
    }

    pub fn slo(&mut self, address: u16, operand: u8) {
        self.asl(address, operand, false);
        self.ora(self.bus.read(address));
    }

    pub fn sre(&mut self, address: u16, operand: u8) {
        self.lsr(address, operand, false);
        self.eor(self.bus.read(address));
    }

    pub fn tas(&mut self, address: u16) {
        let high_byte = (((address & 0xFF00) >> 8) + 1) as u8;
        self.stack_pointer = self.accumulator & self.x;
        self.bus.write(address, self.accumulator & self.x & high_byte);
    }

    pub fn usbc(&mut self, operand: u8) {
        self.sbc(operand);
    }
}

