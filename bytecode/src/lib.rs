#[repr(u8)]
#[derive(Copy, Clone, Debug)]
pub enum OpCode {
    OpReturn = 1,

    // Constants
    OpConstant = 2,
    OpConstantLong = 3,

    // Unary Operations
    OpNegate = 4,
    OpNot = 15,

    // Binary Operations
    OpAdd = 5,
    OpSubtract = 6,
    OpMultiply = 7,
    OpDivide = 8,

    OpGeq = 16,
    OpGt = 17,
    OpLeq = 18,
    OpLt = 19,
    OpEq = 20,
    OpNeq = 21,

    // Other
    OpPrint = 9,
    OpPop = 10,

    // Locals, though
    // Each take 1 operand, which is the index in the stack where
    // the variable lives. Note that if we have >255 variables in
    // scope at once, this representation blows up.
    OpGetLocal = 11, // access the value at this byte
    OpSetLocal = 12, // set the value at this byte; note this is for DEFINITION not reassignment

    // Some magic constant operations
    OpTrue = 13,
    OpFalse = 14,

    // Control flow
    OpJump = 22,            // 2 operands; 2-byte "jump by this (two-byte) amount"
    OpJumpIfFalsePeek = 23, // 2 operands; 2-byte jump amount (peek at stack and maybe jump but don't pop)
    OpJumpIfFalsePop = 24,  // 2 operands; 2-byte jump amount (pop from stack and maybe jump)
    OpJumpBack = 25,        // 2 operands; 2-byte "jump BACKWARDS this amount"

    // Function calls
    OpCall = 26, // 1 operand, which is number of args
}

impl std::convert::TryFrom<u8> for OpCode {
    type Error = u8;
    fn try_from(val: u8) -> Result<OpCode, u8> {
        match val {
            1 => Ok(OpCode::OpReturn),
            2 => Ok(OpCode::OpConstant),
            3 => Ok(OpCode::OpConstantLong),
            4 => Ok(OpCode::OpNegate),
            5 => Ok(OpCode::OpAdd),
            6 => Ok(OpCode::OpSubtract),
            7 => Ok(OpCode::OpMultiply),
            8 => Ok(OpCode::OpDivide),
            9 => Ok(OpCode::OpPrint),
            10 => Ok(OpCode::OpPop),
            11 => Ok(OpCode::OpGetLocal),
            12 => Ok(OpCode::OpSetLocal),
            13 => Ok(OpCode::OpTrue),
            14 => Ok(OpCode::OpFalse),
            15 => Ok(OpCode::OpNot),
            16 => Ok(OpCode::OpGeq),
            17 => Ok(OpCode::OpGt),
            18 => Ok(OpCode::OpLeq),
            19 => Ok(OpCode::OpLt),
            20 => Ok(OpCode::OpEq),
            21 => Ok(OpCode::OpNeq),
            22 => Ok(OpCode::OpJump),
            23 => Ok(OpCode::OpJumpIfFalsePeek),
            24 => Ok(OpCode::OpJumpIfFalsePop),
            25 => Ok(OpCode::OpJumpBack),
            26 => Ok(OpCode::OpCall),
            _ => Err(val),
        }
    }
}

impl From<OpCode> for u8 {
    fn from(val: OpCode) -> u8 {
        val as u8
    }
}

impl OpCode {
    // Gets the number of bytes for this OpCode, including self and any operands
    pub fn num_bytes(self) -> usize {
        match self {
            OpCode::OpCall => 2,
            OpCode::OpReturn => 1,

            // constant lookups
            OpCode::OpConstant => 2,
            OpCode::OpConstantLong => 4,

            // unary operations
            OpCode::OpNegate => 1,
            OpCode::OpNot => 1,

            // binary operations
            OpCode::OpAdd => 1,
            OpCode::OpSubtract => 1,
            OpCode::OpMultiply => 1,
            OpCode::OpDivide => 1,
            OpCode::OpGeq => 1,
            OpCode::OpGt => 1,
            OpCode::OpLeq => 1,
            OpCode::OpLt => 1,
            OpCode::OpEq => 1,
            OpCode::OpNeq => 1,

            // Other
            OpCode::OpPrint => 1,
            OpCode::OpPop => 1,

            // Locals stuff; operand is place on the stack to put the value
            OpCode::OpGetLocal => 2,
            OpCode::OpSetLocal => 2,

            OpCode::OpTrue => 1,
            OpCode::OpFalse => 1,

            OpCode::OpJump => 3,
            OpCode::OpJumpIfFalsePeek => 3,
            OpCode::OpJumpIfFalsePop => 3,
            OpCode::OpJumpBack => 3,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Function(FnValue),
}

/// Function object, stored as a first-class value in Lox
/// Note: I really really don't like this representation (very heavyweight, breaks Copy, etc.)
/// but let's follow the book and get it working, then test, then think about how to fix it
#[derive(Clone)]
pub struct FnValue {
    pub name: String,
    pub arity: usize,
    pub chunk: Chunk,
}

impl std::fmt::Debug for FnValue {
    fn fmt<'a>(&self, f: &mut std::fmt::Formatter<'a>) -> std::fmt::Result {
        write!(f, "Function {}: {} args", self.name, self.arity)
    }
}

impl Value {
    pub fn as_kind(&self) -> &'static str {
        match self {
            Value::Int(_) => "int",
            Value::Bool(_) => "bool",
            Value::Function(_) => "fn",
        }
    }
}

#[derive(Clone)]
pub struct Chunk {
    // Stored contiguously; some of these are OpCodes, and some are operands
    code: Vec<u8>,
    // Some bytes in code are casted to indices in values; this is the constant pool
    values: Vec<Value>,
    // TODO: do a run-length encoding; currently this is in 1-1 correspondence with code
    lines: Vec<usize>,
}

impl Chunk {
    pub fn init() -> Self {
        Chunk {
            code: Vec::new(),
            values: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn push_value(&mut self, val: Value) -> usize {
        let new_index = self.values.len();
        self.values.push(val);
        new_index
    }

    pub fn push_code<T>(&mut self, code: T, line: usize)
    where
        T: Into<u8>,
    {
        self.code.push(code.into());
        self.lines.push(line);
    }

    pub fn set_code<T>(&mut self, code: T, offset: usize)
    where
        T: Into<u8>,
    {
        self.code[offset] = code.into();
    }

    #[inline(always)]
    pub fn get_code(&self) -> &[u8] {
        &self.code
    }

    pub fn get_line(&self, code_offset: usize) -> usize {
        self.lines[code_offset]
    }

    #[inline(always)]
    pub fn get_value(&self, index: usize) -> Option<Value> {
        // TODO: turn this back into copied
        self.values.get(index).cloned()
    }

    #[inline(always)]
    pub fn get_two_bytes(&self, index: usize) -> usize {
        ((self.code[index] as usize) << 8) + (self.code[index + 1] as usize)
    }

    #[inline(always)]
    pub fn get_three_bytes(&self, index: usize) -> usize {
        ((self.code[index] as usize) << 16)
            + ((self.code[index + 1] as usize) << 8)
            + ((self.code[index + 2]) as usize)
    }
}

pub mod disassemble {
    use std::convert::TryInto;

    use super::{Chunk, OpCode};

    pub fn disassemble_chunk(chunk: &Chunk, name: &str) -> String {
        let mut out = String::new();

        out.push_str(&format!("=== {} ===\n", name));

        let mut index = 0;

        while index < chunk.code.len() {
            // TODO: do the "same as last instruction line | thing"
            let new_str = disassemble_instruction(&mut index, chunk);
            out.push_str(&new_str);
        }

        out
    }

    fn disassemble_instruction(offset_ptr: &mut usize, chunk: &Chunk) -> String {
        let offset = *offset_ptr;

        let code = chunk.code[offset];
        let line = chunk.get_line(offset);

        let op_code = code.try_into();
        let string_out = match op_code {
            Ok(OpCode::OpCall) => {
                (format!(
                    "{:04} {:04} OP_CALL ({} args)\n",
                    line,
                    offset,
                    chunk.code[offset + 1]
                ))
            }
            Ok(OpCode::OpReturn) => (format!("{:04} {:04} OP_RETURN\n", line, offset)),
            Ok(OpCode::OpNegate) => (format!("{:04} {:04} OP_NEGATE\n", line, offset)),
            Ok(OpCode::OpGeq) => (format!("{:04} {:04} OP_GEQ\n", line, offset)),
            Ok(OpCode::OpGt) => (format!("{:04} {:04} OP_GT\n", line, offset)),
            Ok(OpCode::OpLeq) => (format!("{:04} {:04} OP_LEQ\n", line, offset)),
            Ok(OpCode::OpLt) => (format!("{:04} {:04} OP_LT\n", line, offset)),
            Ok(OpCode::OpEq) => (format!("{:04} {:04} OP_EQ\n", line, offset)),
            Ok(OpCode::OpNeq) => (format!("{:04} {:04} OP_NEQ\n", line, offset)),
            Ok(OpCode::OpNot) => (format!("{:04} {:04} OP_NOT\n", line, offset)),
            Ok(OpCode::OpAdd) => (format!("{:04} {:04} OP_ADD\n", line, offset)),
            Ok(OpCode::OpSubtract) => (format!("{:04} {:04} OP_SUBTRACT\n", line, offset)),
            Ok(OpCode::OpMultiply) => (format!("{:04} {:04} OP_MULTIPLY\n", line, offset)),
            Ok(OpCode::OpDivide) => (format!("{:04} {:04} OP_DIVIDE\n", line, offset)),
            Ok(OpCode::OpPrint) => (format!("{:04} {:04} OP_PRINT\n", line, offset)),
            Ok(OpCode::OpPop) => (format!("{:04} {:04} OP_POP\n", line, offset)),
            Ok(OpCode::OpTrue) => (format!("{:04} {:04} OP_TRUE\n", line, offset)),
            Ok(OpCode::OpFalse) => (format!("{:04} {:04} OP_FALSE\n", line, offset)),
            Ok(OpCode::OpGetLocal) => {
                (format!(
                    "{:04} {:04} OP_GET_LOCAL {}\n",
                    line,
                    offset,
                    chunk.code[offset + 1]
                ))
            }
            Ok(OpCode::OpSetLocal) => {
                (format!(
                    "{:04} {:04} OP_SET_LOCAL {}\n",
                    line,
                    offset,
                    chunk.code[offset + 1]
                ))
            }
            Ok(OpCode::OpConstant) => {
                let value_index = chunk.code[offset + 1];
                let value = chunk.get_value(value_index as usize);
                match value {
                    Some(v) => (format!("{:04} {:04} OP_CONSTANT -- {:?}\n", line, offset, v)),
                    None => {
                        (format!(
                            "{:04} OP_CONSTANT -- index {} which is missing\n",
                            offset, value_index
                        ))
                    }
                }
            }
            Ok(OpCode::OpConstantLong) => {
                let value_index = chunk.get_three_bytes(offset + 1);
                let value = chunk.get_value(value_index);
                match value {
                    Some(v) => (format!("{:04} {:04} OP_CONSTANT_LONG -- {:?}\n", line, offset, v)),
                    None => {
                        (format!(
                            "{:04} OP_CONSTANT_LONG -- index {} which is missing\n",
                            offset, value_index
                        ))
                    }
                }
            }
            Ok(OpCode::OpJump) => {
                let jump_offset = chunk.get_two_bytes(offset + 1);
                format!(
                    "{:04} {:04} OP_JUMP by {} (+3)\n",
                    line, offset, jump_offset
                )
            }
            Ok(OpCode::OpJumpBack) => {
                let jump_offset = chunk.get_two_bytes(offset + 1);
                format!(
                    "{:04} {:04} OP_JUMP_BACK by {} (-3)\n",
                    line, offset, jump_offset
                )
            }
            Ok(OpCode::OpJumpIfFalsePeek) => {
                let jump_offset = chunk.get_two_bytes(offset + 1);
                format!(
                    "{:04} {:04} OP_JUMP_IF_FALSE_PEEK by {} (+3)\n",
                    line, offset, jump_offset
                )
            }
            Ok(OpCode::OpJumpIfFalsePop) => {
                let jump_offset = chunk.get_two_bytes(offset + 1);
                format!(
                    "{:04} {:04} OP_JUMP_IF_FALSE_POP by {} (+3)\n",
                    line, offset, jump_offset
                )
            }
            Err(val) => (format!("{:04} {:04} Unrecognized code: {}\n", line, offset, val)),
        };

        *offset_ptr += match op_code {
            Ok(oc) => oc.num_bytes(),
            // Err is probably unrecoverable bad news but we have to do _something_
            Err(_) => 1,
        };

        (string_out)
    }
}

// TODO: much of this is unit testable
