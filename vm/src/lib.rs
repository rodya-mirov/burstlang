use std::convert::TryInto;

use bytecode::{Chunk, OpCode, Value};

pub struct VM<'a> {
    chunk: &'a Chunk,
    stack: Vec<Value>,
    // instruction pointer; name is classical
    ip: usize,
}

impl<'a> VM<'a> {
    pub fn init(chunk: &'a Chunk) -> Self {
        VM {
            chunk,
            stack: Vec::new(),
            ip: 0,
        }
    }

    pub fn get_stack(&self) -> &[Value] {
        &self.stack
    }

    pub fn run(&mut self) {
        let chunk: &Chunk = &self.chunk;
        let code = chunk.get_code();

        loop {
            let op_code: OpCode = code[self.ip].try_into().expect("Byte should be an OpCode");

            match op_code {
                OpCode::OpReturn => {
                    break;
                }
                OpCode::OpPrint => {
                    let stack_val = self.stack.pop().unwrap();
                    println!("{:?}", stack_val.0);
                }
                OpCode::OpPop => {
                    let _ = self.stack.pop().unwrap();
                }
                OpCode::OpGetLocal => {
                    let offset = code[self.ip + 1] as usize;
                    let val = self.stack[offset];
                    self.stack.push(val);
                }
                OpCode::OpSetLocal => {
                    // NB: if the variable has never been set (as is the
                    // case always for now, since variables are immutable),
                    // then this is a no-op -- the value on the stack is popped
                    // off, then put in the reserved slot, which is right where
                    // we are.
                }
                OpCode::OpConstant => {
                    let value_index = code[self.ip + 1] as usize;
                    let value = chunk.get_value(value_index).expect("Value should exist");
                    self.stack.push(value);
                }
                OpCode::OpConstantLong => {
                    let value_index = bytecode::make_three_byte_index(
                        code[self.ip + 1],
                        code[self.ip + 2],
                        code[self.ip + 3],
                    );
                    let value = chunk.get_value(value_index).expect("Value should exist");
                    self.stack.push(value);
                }
                OpCode::OpNegate => {
                    self.unary_op(neg_op);
                }
                OpCode::OpAdd => {
                    self.binary_op(add_op);
                }
                OpCode::OpSubtract => {
                    self.binary_op(sub_op);
                }
                OpCode::OpMultiply => {
                    self.binary_op(mul_op);
                }
                OpCode::OpDivide => {
                    self.binary_op(div_op);
                }
            }

            self.ip += op_code.num_bytes();
        }
    }

    fn unary_op<F>(&mut self, f: F)
    where
        F: Fn(Value) -> Value,
    {
        let old = self.stack.pop().expect("Stack underflow");
        self.stack.push(f(old));
    }

    fn binary_op<F>(&mut self, f: F)
    where
        F: Fn(Value, Value) -> Value,
    {
        let b = self.stack.pop().expect("Stack underflow");
        let a = self.stack.pop().expect("Stack underflow");
        self.stack.push(f(a, b));
    }
}

fn neg_op(a: Value) -> Value {
    Value(-a.0)
}

fn add_op(a: Value, b: Value) -> Value {
    Value(a.0 + b.0)
}

fn sub_op(a: Value, b: Value) -> Value {
    Value(a.0 - b.0)
}

fn mul_op(a: Value, b: Value) -> Value {
    Value(a.0 * b.0)
}

fn div_op(a: Value, b: Value) -> Value {
    Value(a.0 / b.0)
}
