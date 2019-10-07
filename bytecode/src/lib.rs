#[repr(u8)]
#[derive(Copy, Clone)]
pub enum OpCode {
    OpReturn = 1,

    // Constants
    OpConstant = 2,
    OpConstantLong = 3,

    // Unary Operations
    OpNegate = 4,

    // Binary Operations
    OpAdd = 5,
    OpSubtract = 6,
    OpMultiply = 7,
    OpDivide = 8,
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
            OpCode::OpReturn => 1,

            // constant lookups
            OpCode::OpConstant => 2,
            OpCode::OpConstantLong => 4,

            // unary operations
            OpCode::OpNegate => 1,

            // binary operations
            OpCode::OpAdd => 1,
            OpCode::OpSubtract => 1,
            OpCode::OpMultiply => 1,
            OpCode::OpDivide => 1,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Value(pub i64);

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

    #[inline(always)]
    pub fn get_code(&self) -> &[u8] {
        &self.code
    }

    pub fn get_line(&self, code_offset: usize) -> usize {
        self.lines[code_offset]
    }

    #[inline(always)]
    pub fn get_value(&self, index: usize) -> Option<Value> {
        self.values.get(index).copied()
    }
}

pub mod disassemble {
    use std::convert::TryInto;

    use super::{make_three_byte_index, Chunk, OpCode};

    pub fn disassemble_chunk(chunk: &Chunk, name: &str) -> String {
        let mut out = String::new();

        out.push_str(&format!("=== {} ===", name));

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
            Ok(OpCode::OpReturn) => (format!("{:04} {:04} OP_RETURN", line, offset)),
            Ok(OpCode::OpNegate) => (format!("{:04} {:04} OP_NEGATE", line, offset)),
            Ok(OpCode::OpAdd) => (format!("{:04} {:04} OP_ADD", line, offset)),
            Ok(OpCode::OpSubtract) => (format!("{:04} {:04} OP_SUBTRACT", line, offset)),
            Ok(OpCode::OpMultiply) => (format!("{:04} {:04} OP_MULTIPLY", line, offset)),
            Ok(OpCode::OpDivide) => (format!("{:04} {:04} OP_DIVIDE", line, offset)),
            Ok(OpCode::OpConstant) => {
                let value_index = chunk.code[offset + 1];
                let value = chunk.get_value(value_index as usize);
                match value {
                    Some(v) => (format!("{:04} {:04} OP_CONSTANT -- {}", line, offset, v.0)),
                    None => {
                        (format!(
                            "{:04} OP_CONSTANT -- index {} which is missing",
                            offset, value_index
                        ))
                    }
                }
            }
            Ok(OpCode::OpConstantLong) => {
                let value_index = make_three_byte_index(
                    chunk.code[offset + 1],
                    chunk.code[offset + 2],
                    chunk.code[offset + 3],
                );
                let value = chunk.get_value(value_index);
                match value {
                    Some(v) => (format!("{:04} {:04} OP_CONSTANT_LONG -- {}", line, offset, v.0)),
                    None => {
                        (format!(
                            "{:04} OP_CONSTANT_LONG -- index {} which is missing",
                            offset, value_index
                        ))
                    }
                }
            }
            Err(val) => (format!("{:04} {:04} Unrecognized code: {}", line, offset, val)),
        };

        *offset_ptr += match op_code {
            Ok(oc) => oc.num_bytes(),
            // Err is probably unrecoverable bad news but we have to do _something_
            Err(_) => 1,
        };

        (string_out)
    }
}

pub fn make_three_byte_index(a: u8, b: u8, c: u8) -> usize {
    ((a as usize) << 16) + ((b as usize) << 8) + (c as usize)
}

// TODO: much of this is unit testable
