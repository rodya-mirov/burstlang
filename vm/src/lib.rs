use std::convert::TryInto;

use bytecode::{Chunk, OpCode, Value};

pub struct VM<'a> {
    chunk: &'a Chunk,
    stack: Vec<Value>,
    globals: Vec<Option<Value>>,
    // instruction pointer; name is classical
    ip: usize,
}

impl<'a> VM<'a> {
    pub fn init(chunk: &'a Chunk) -> Self {
        VM {
            chunk,
            stack: Vec::new(),
            globals: (0..chunk.num_globals()).map(|_| None).collect(),
            ip: 0,
        }
    }

    pub fn get_stack(&self) -> &[Value] {
        &self.stack
    }

    pub fn make_global_map(&self) -> Vec<(&str, Option<Value>)> {
        let mut out = Vec::with_capacity(self.globals.len());
        for i in 0..self.globals.len() {
            out.push((
                self.chunk.get_global_name(i).unwrap(),
                self.globals[i].clone(),
            ));
        }
        out
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
                OpCode::OpDefine => {
                    let val = self.stack.pop().unwrap();
                    let global_index = code[self.ip + 1] as usize;
                    self.globals[global_index] = Some(val);
                }
                OpCode::OpVariableAccess => {
                    let global_index = code[self.ip + 1] as usize;
                    let global_value = self.globals[global_index];

                    if global_value.is_none() {
                        panic!(
                            "Variable {} (index {}) accessed before use.",
                            self.chunk.get_global_name(global_index).unwrap(),
                            global_index
                        );
                    }

                    self.stack.push(global_value.unwrap());
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
