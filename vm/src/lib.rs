use std::convert::TryInto;

use bytecode::{Chunk, FnValue, OpCode, Value};

pub struct VM {
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
}

pub struct CallFrame {
    // Offset in the (full) stack to start indexing our locals into
    local_offset: usize,
    // How many locals to pop off the stack when we return
    function_arity: usize,
    // Which chunk we are executing
    chunk: Chunk,
    // instruction pointer; name is classical
    ip: usize,
}

pub trait Executor {
    fn print(&mut self, val: Value);
}

impl VM {
    pub fn init(chunk: Chunk) -> Self {
        VM {
            // TODO: make these into smallvecs that explode instead of resizing
            stack: Vec::new(),
            // TODO: make these into smallvecs that explode instead of resizing
            frames: vec![CallFrame {
                local_offset: 0,
                function_arity: 0,
                chunk,
                ip: 0,
            }],
        }
    }

    pub fn get_stack(&self) -> &[Value] {
        &self.stack
    }

    pub fn run<T: Executor>(&mut self, executor: &mut T) {
        let tree = bytecode::disassemble::disassemble_chunk(
            &self.frames.last().unwrap().chunk,
            "main_chunk",
        );
        println!("{}", tree);

        loop {
            let frame: &mut CallFrame = self.frames.last_mut().unwrap();
            let chunk = &frame.chunk;
            let code = chunk.get_code();

            let op_code: OpCode = code[frame.ip].try_into().expect("Byte should be an OpCode");

            // stdout gives helpful debugger output, hopefully, for failing tests (it has helped before)

            println!("Executing at byte {} which is code {:?}", frame.ip, op_code);

            // Macros don't require function calls or closure objects, and don't hold onto lifetimes
            // But you can "construct" them inside the loop like this so we can "enclose" state <3
            macro_rules! and_inc {
                ($action:tt) => {{
                    use std::num::Wrapping;

                    $action;
                    frame.ip = (Wrapping(frame.ip) + Wrapping(op_code.num_bytes())).0;
                }};
            }

            macro_rules! unary_op {
                ($f:expr) => {{
                    use std::num::Wrapping;

                    let old = self.stack.pop().expect("Stack underflow");
                    self.stack.push($f(old));

                    frame.ip = (Wrapping(frame.ip) + Wrapping(op_code.num_bytes())).0;
                }};
            }

            macro_rules! binary_op {
                ($f:expr) => {{
                    use std::num::Wrapping;

                    let b = self.stack.pop().expect("Stack underflow");
                    let a = self.stack.pop().expect("Stack underflow");
                    self.stack.push($f(a, b));

                    frame.ip = (Wrapping(frame.ip) + Wrapping(op_code.num_bytes())).0;
                }};
            }

            match op_code {
                OpCode::OpCall => {
                    let function_arity = code[frame.ip + 1] as usize;
                    let func_ptr: FnValue =
                        as_func(self.stack[self.stack.len() - function_arity - 1].clone());
                    let func_chunk = func_ptr.chunk.clone();

                    // Note: we modify frame.ip now, so that when we return, we'll be at the right place
                    frame.ip += op_code.num_bytes();

                    self.frames.push(CallFrame {
                        local_offset: self.frames.last().unwrap().local_offset + function_arity + 1,
                        function_arity,
                        chunk: func_chunk,
                        ip: 0,
                    });
                }
                OpCode::OpReturn => {
                    // If we're not in a function, OpReturn means "stop program execution"
                    if self.frames.len() == 1 {
                        break;
                    }

                    let return_val = self.stack.pop().unwrap();

                    let returning_frame = self.frames.pop().unwrap();

                    for _ in 0..(returning_frame.function_arity + 1) {
                        // pop the function arguments and the function itself
                        self.stack.pop();
                    }

                    // The return value should end up on the stack
                    self.stack.push(return_val);
                }
                OpCode::OpPrint => and_inc!({
                    let stack_val = self.stack.pop().unwrap();
                    executor.print(stack_val);
                }),
                OpCode::OpPop => and_inc!({
                    let _ = self.stack.pop().unwrap();
                }),
                OpCode::OpJump => and_inc!({
                    let jump_dist = chunk.get_two_bytes(frame.ip + 1);
                    frame.ip += jump_dist;
                }),
                OpCode::OpJumpBack => and_inc!({
                    let jump_dist = chunk.get_two_bytes(frame.ip + 1);
                    // subtract with overflow; note we want to wrap around, on purpose
                    frame.ip = (Wrapping(frame.ip) - Wrapping(jump_dist)).0;
                }),
                OpCode::OpJumpIfFalsePeek => and_inc!({
                    //let peeked = as_bool(*self.stack.last().unwrap());
                    // TODO: go back to the above once we get copy back
                    let peeked = as_bool(self.stack.last().cloned().unwrap());
                    if !peeked {
                        let jump_dist = chunk.get_two_bytes(frame.ip + 1);
                        frame.ip += jump_dist;
                    }
                }),
                OpCode::OpJumpIfFalsePop => and_inc!({
                    let peeked = as_bool(self.stack.pop().unwrap());
                    if !peeked {
                        let jump_dist = chunk.get_two_bytes(frame.ip + 1);
                        frame.ip += jump_dist;
                    }
                }),
                OpCode::OpGetLocal => and_inc!({
                    let offset = code[frame.ip + 1] as usize;
                    // let val = self.stack[offset];
                    // TODO: go back to the above once we get copy back
                    let val = self
                        .stack
                        .get(offset + frame.local_offset)
                        .cloned()
                        .unwrap();
                    self.stack.push(val);
                }),
                OpCode::OpSetLocal => and_inc!({
                    // NB: if the variable has never been set (as is the
                    // case always for now, since variables are immutable),
                    // then this is a no-op -- the value on the stack is popped
                    // off, then put in the reserved slot, which is right where
                    // we are.
                }),
                OpCode::OpConstant => and_inc!({
                    let value_index = code[frame.ip + 1] as usize;
                    let value = chunk.get_value(value_index).expect("Value should exist");
                    self.stack.push(value);
                }),
                OpCode::OpTrue => and_inc!({ self.stack.push(Value::Bool(true)) }),
                OpCode::OpFalse => and_inc!({ self.stack.push(Value::Bool(false)) }),
                OpCode::OpConstantLong => and_inc!({
                    let value_index = chunk.get_three_bytes(frame.ip + 1);
                    let value = chunk.get_value(value_index).expect("Value should exist");
                    self.stack.push(value);
                }),
                OpCode::OpNot => unary_op!(not_op),
                OpCode::OpNegate => unary_op!(neg_op),
                OpCode::OpAdd => binary_op!(add_op),
                OpCode::OpSubtract => binary_op!(sub_op),
                OpCode::OpMultiply => binary_op!(mul_op),
                OpCode::OpDivide => binary_op!(div_op),
                OpCode::OpGeq => binary_op!(geq_op),
                OpCode::OpGt => binary_op!(gt_op),
                OpCode::OpLeq => binary_op!(leq_op),
                OpCode::OpLt => binary_op!(lt_op),
                OpCode::OpEq => binary_op!(eq_op),
                OpCode::OpNeq => binary_op!(neq_op),
            }
        }
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
fn as_func(value: Value) -> FnValue {
    match value {
        Value::Function(v) => v,
        _ => type_error("function", value),
    }
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
        // TODO: functions should not implement Eq and compiler should prevent this
        Value::Function(_) => panic!("Compiler error; should prevent fn == fn at compile time"),
    };
    Value::Bool(out)
}

fn neq_op(a: Value, b: Value) -> Value {
    let out = match a {
        Value::Int(v) => v != as_int(b),
        Value::Bool(v) => v != as_bool(b),
        // TODO: functions should not implement Eq and compiler should prevent this
        Value::Function(_) => panic!("Compiler error; should prevent fn == fn at compile time"),
    };
    Value::Bool(out)
}
