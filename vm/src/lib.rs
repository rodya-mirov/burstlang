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
        use std::num::Wrapping;

        let chunk: &Chunk = &self.chunk;
        let code = chunk.get_code();

        loop {
            let op_code: OpCode = code[self.ip].try_into().expect("Byte should be an OpCode");
            println!("Executing at byte {} which is code {:?}", self.ip, op_code);

            match op_code {
                OpCode::OpReturn => {
                    println!("RETURN");
                    break;
                }
                OpCode::OpPrint => {
                    let stack_val = self.stack.pop().unwrap();
                    println!("{:?}", stack_val);
                }
                OpCode::OpPop => {
                    let _ = self.stack.pop().unwrap();
                }
                OpCode::OpJump => {
                    let jump_dist = chunk.get_two_bytes(self.ip + 1);
                    self.ip += jump_dist;
                }
                OpCode::OpJumpBack => {
                    let jump_dist = chunk.get_two_bytes(self.ip + 1);
                    // subtract with overflow; note we want to wrap around, on purpose
                    self.ip = (Wrapping(self.ip) - Wrapping(jump_dist)).0;
                }
                OpCode::OpJumpIfFalsePeek => {
                    let peeked = as_bool(*self.stack.last().unwrap());
                    if !peeked {
                        let jump_dist = chunk.get_two_bytes(self.ip + 1);
                        self.ip += jump_dist;
                    }
                }
                OpCode::OpJumpIfFalsePop => {
                    let peeked = as_bool(self.stack.pop().unwrap());
                    if !peeked {
                        let jump_dist = chunk.get_two_bytes(self.ip + 1);
                        self.ip += jump_dist;
                    }
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
                OpCode::OpTrue => self.stack.push(Value::Bool(true)),
                OpCode::OpFalse => self.stack.push(Value::Bool(false)),
                OpCode::OpConstantLong => {
                    let value_index = chunk.get_three_bytes(self.ip + 1);
                    let value = chunk.get_value(value_index).expect("Value should exist");
                    self.stack.push(value);
                }
                OpCode::OpNot => self.unary_op(not_op),
                OpCode::OpNegate => self.unary_op(neg_op),
                OpCode::OpAdd => self.binary_op(add_op),
                OpCode::OpSubtract => self.binary_op(sub_op),
                OpCode::OpMultiply => self.binary_op(mul_op),
                OpCode::OpDivide => self.binary_op(div_op),
                OpCode::OpGeq => self.binary_op(geq_op),
                OpCode::OpGt => self.binary_op(gt_op),
                OpCode::OpLeq => self.binary_op(leq_op),
                OpCode::OpLt => self.binary_op(lt_op),
                OpCode::OpEq => self.binary_op(eq_op),
                OpCode::OpNeq => self.binary_op(neq_op),
            }

            self.ip = (Wrapping(self.ip) + Wrapping(op_code.num_bytes())).0;
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

fn type_error(expected: &str, actual: Value) -> ! {
    panic!(
        "When evaluating an expression, got '{}' but expected '{}'",
        actual.as_kind(),
        expected
    )
}

#[inline(always)]
fn as_int(value: Value) -> i64 {
    match value {
        Value::Int(v) => v,
        _ => type_error("int", value),
    }
}

#[inline(always)]
fn as_bool(value: Value) -> bool {
    match value {
        Value::Bool(b) => b,
        _ => type_error("bool", value),
    }
}

fn not_op(a: Value) -> Value {
    Value::Bool(!as_bool(a))
}

fn neg_op(a: Value) -> Value {
    Value::Int(-as_int(a))
}

fn add_op(a: Value, b: Value) -> Value {
    Value::Int(as_int(a) + as_int(b))
}

fn sub_op(a: Value, b: Value) -> Value {
    Value::Int(as_int(a) - as_int(b))
}

fn mul_op(a: Value, b: Value) -> Value {
    Value::Int(as_int(a) * as_int(b))
}

fn div_op(a: Value, b: Value) -> Value {
    Value::Int(as_int(a) / as_int(b))
}

fn geq_op(a: Value, b: Value) -> Value {
    Value::Bool(as_int(a) >= as_int(b))
}

fn gt_op(a: Value, b: Value) -> Value {
    Value::Bool(as_int(a) > as_int(b))
}

fn leq_op(a: Value, b: Value) -> Value {
    Value::Bool(as_int(a) <= as_int(b))
}

fn lt_op(a: Value, b: Value) -> Value {
    Value::Bool(as_int(a) < as_int(b))
}

fn eq_op(a: Value, b: Value) -> Value {
    let out = match a {
        Value::Int(v) => v == as_int(b),
        Value::Bool(v) => v == as_bool(b),
    };
    Value::Bool(out)
}

fn neq_op(a: Value, b: Value) -> Value {
    let out = match a {
        Value::Int(v) => v != as_int(b),
        Value::Bool(v) => v != as_bool(b),
    };
    Value::Bool(out)
}
