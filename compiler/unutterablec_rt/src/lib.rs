use std::ops::{Index, IndexMut};

#[derive(Debug, Clone, Copy)]
pub struct Word(u64);

#[derive(Debug)]
pub struct Stack {
    inner: Vec<Word>,
}

#[derive(Debug)]
pub struct Registers {
    regs: [Word; Reg::variant_count()],
}

#[derive(Debug)]
pub struct Heap {
    dense: Vec<usize>,
    sparse: Vec<Word>,
}

#[derive(Debug)]
pub struct VM {
    regs: Registers,
    stack: Stack,
    heap: Heap,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Reg {
    RIP,
    /// `rax`
    R0,
    /// `rbx`
    R1,
    /// `rcx`
    R2,
    /// `rdx`
    R3,
    /// `rsi`
    R4,
    /// `rdi`
    R5,
    /// `rbp`
    R6,
    /// `rsp`
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

pub use Reg::*;

#[derive(Debug)]
pub struct Mem(u64);

#[derive(Debug)]
pub struct Imm(u64);

#[derive(Debug)]
pub enum DestOp {
    Reg(Reg),
    Mem(Mem),
}

#[derive(Debug)]
pub enum SrcOp {
    Reg(Reg),
    Mem(Mem),
    Imm(Imm),
}

#[derive(Debug)]
pub enum Syscall {}

#[derive(Debug)]
pub enum Insn {
    Mov(DestOp, SrcOp),
}

#[derive(Debug)]
pub struct Bytecode {
    insns: Vec<Insn>,
}

impl Stack {
    pub fn new() -> Stack {
        Stack { inner: Vec::new() }
    }

    pub fn reserve(&mut self, n: usize) {
        self.inner.reserve_exact(n);
    }
}

impl Registers {
    pub fn new() -> Registers {
        Registers {
            regs: [Word(0); Reg::variant_count()],
        }
    }
}

impl Heap {
    pub fn new() -> Heap {
        Heap {
            dense: Vec::new(),
            sparse: Vec::new(),
        }
    }
}

impl VM {
    pub fn new() -> VM {
        VM {
            regs: Registers::new(),
            stack: Stack::new(),
            heap: Heap::new(),
        }
    }
}

impl Reg {
    // One day, Rust. One day...
    // https://doc.rust-lang.org/std/mem/fn.variant_count.html
    const fn variant_count() -> usize {
        17
    }
}

impl Index<Reg> for Registers {
    type Output = Word;

    fn index(&self, index: Reg) -> &Self::Output {
        &self.regs[index as usize]
    }
}

impl IndexMut<Reg> for Registers {
    fn index_mut(&mut self, index: Reg) -> &mut Self::Output {
        &mut self.regs[index as usize]
    }
}
