use crate::api::{LuaValue, LuaApi, LuaVM, LuaTable, Closure};
use std::sync::Arc;
use crate::arith::{ArithOp, arith, CompareOp, compare};
use crate::vm::PCAddr;
use crate::chunk::{ProtoType};
use crate::instruction::{RealInstruction, Instruction, RawOp};
use std::cell::RefCell;
use bytes::Bytes;

struct LuaStack {
    slots: Vec<LuaValue>,
    // slots
    closure: Arc<Closure>,
    var_args: Vec<LuaValue>,
    pc: PCAddr,
}


impl LuaStack {
    pub fn new(size: usize,closure:Arc<Closure>) -> LuaStack {
        LuaStack {
            slots: Vec::with_capacity(size),
            closure,
            var_args: vec![],
            pc: 0,
        }
    }

    pub fn top(&self) -> isize {
        self.slots.len() as isize
    }
    pub fn abs_top(&self) -> usize {
        self.abs_index(self.top())
    }

    pub fn check(&mut self, n: usize) {
        self.slots.reserve(n);
    }

    pub fn push(&mut self, val: LuaValue) {
        self.slots.push(val);
    }

    pub fn pop(&mut self) -> LuaValue {
        self.slots.pop().unwrap()
    }

    pub fn pop_n(&mut self, n: usize) -> Vec<LuaValue> {
        let mut vec = Vec::with_capacity(n);
        for _ in 0..n {
            vec.push(self.pop());
        }
        vec.reverse();
        vec
    }

    pub fn push_n(&mut self, mut vals: Vec<LuaValue>, n: isize) {
        vals.reverse();
        let val_len = vals.len();
        let un = if n < 0 { val_len } else { n as usize };

        for i in 0..un {
            if i < val_len {
                self.push(vals.pop().unwrap());
            } else {
                self.push(LuaValue::Nil);
            }
        }
    }

    pub fn set_top(&mut self, idx: isize) {
        let new_top = self.abs_index(idx);
        let old_top = self.abs_top();
        if new_top < 0 {
            panic!("stack underflow!");
        }

        if old_top > new_top {
            for _ in 0..(old_top - new_top) {
                self.pop();
            }
        } else {
            for _ in 0..(new_top - old_top) {
                self.push(LuaValue::Nil);
            }
        }
    }

    pub fn abs_index(&self, idx: isize) -> usize {
        if idx >= 0 {
            idx as usize
        } else {
            (idx + self.top() + 1) as usize
        }
    }

    pub fn is_valid(&self, idx: isize) -> bool {
        let abs_idx = self.abs_index(idx);
        abs_idx > 0 && abs_idx <= self.abs_top()
    }

    pub fn get(&self, idx: isize) -> LuaValue {
        let abs_idx = self.abs_index(idx);
        if abs_idx > 0 && abs_idx <= self.abs_top() {
            let idx = abs_idx as usize - 1;
            self.slots[idx].clone() // TODO
        } else {
            LuaValue::Nil
        }
    }

    // pub fn peek(&self, idx: isize) -> &LuaValue {
    //     let abs_idx = self.abs_index(idx);
    //     if abs_idx > 0 && abs_idx <= self.abs_top() {
    //         let idx = abs_idx as usize - 1;
    //         &self.slots[idx]
    //     } else {
    //         &LuaValue::Nil
    //     }
    // }

    pub fn set(&mut self, idx: isize, val: LuaValue) {
        let abs_idx = self.abs_index(idx);
        if abs_idx > 0 && abs_idx <= self.abs_top() {
            let idx = abs_idx as usize - 1;
            self.slots[idx] = val;
        } else {
            panic!("invalid index: {}", idx);
        }
    }

    pub fn reverse(&mut self, mut from: usize, mut to: usize) {
        while from < to {
            self.slots.swap(from, to);
            from += 1;
            to -= 1;
        }
    }
}

pub struct LuaState {
    // todo LinkList
    stacks: Vec<LuaStack>,
}

impl LuaState {
    pub fn new() -> LuaState {
        Self::with_capacity(20)
    }
    pub fn with_capacity(i: usize) -> LuaState {
        LuaState { stacks: vec![LuaStack::new(i, Arc::new(Closure::new(Arc::new(ProtoType::new_fake()))))] }
    }
    fn stack_mut(&mut self) -> &mut LuaStack {
        self.stacks.last_mut().expect("must be at least 1 stack.") // TODO ensure at least 1 stack
    }

    fn stack(&self) -> &LuaStack {
        self.stacks.last().expect("must be at least 1 stack.") // TODO ensure at least 1 stack
    }

    fn push_stack(&mut self, frame: LuaStack) {
        self.stacks.push(frame);
    }

    fn pop_stack(&mut self) -> LuaStack {
        self.stacks.pop().unwrap()
    }


    fn get_table_value_by_key(&mut self, t: &LuaValue, k: &LuaValue) -> LuaValue {
        if let LuaValue::Table(tbl) = t {
            let v = tbl.borrow().get(k);
            self.stack_mut().push(v.clone());
            v
        } else {
            panic!("not a table!") // todo
        }
    }

    fn set_table_kv(t: &LuaValue, k: LuaValue, v: LuaValue) {
        if let LuaValue::Table(tbl) = t {
            tbl.borrow_mut().put(k, v);
        } else {
            panic!("not a table!");
        }
    }
    fn call_lua_closure(&mut self, in_argc: usize, out_argc: isize, c: Arc<Closure>) {
        let num_regs = c.proto.max_stack_size as usize;
        let num_params = c.proto.num_params as usize;
        let is_vararg = c.proto.is_vararg == 1;

        // create new lua stack
        let mut new_stack = LuaStack::new(num_regs + 20, c);

        let stack = self.stack_mut();
        // pass args, pop func
        let mut args = stack.pop_n(in_argc);
        stack.pop(); // pop func
        if in_argc > num_params {
            // varargs
            for _i in num_params..in_argc {
                new_stack.var_args.push(args.pop().unwrap());
            }
            if is_vararg {
                new_stack.var_args.reverse();
            } else {
                new_stack.var_args.clear();
            }
        }
        new_stack.push_n(args, num_params as isize);
        new_stack.set_top(num_regs as isize);

        // run closure
        self.push_stack(new_stack);
        self.run_lua_closure();
        new_stack = self.pop_stack();

        // return results
        if out_argc != 0 {
            let nrets = new_stack.top() as usize - num_regs;
            let results = new_stack.pop_n(nrets);
            self.stack_mut().check(nrets);
            self.stack_mut().push_n(results, out_argc);
        }
    }

    fn run_lua_closure(&mut self) {
        loop {
            let instr = Instruction(self.fetch());
            instr.execute(self);
            // print_stack(instr.opname(), self);
            if let RawOp::RETURN = instr.raw_op_code() {
                break;
            }
        }
    }
}

impl LuaVM for LuaState {
    fn pc(&self) -> PCAddr {
        self.stack().pc
    }

    fn add_pc(&mut self, n: isize) {
        self.stack_mut().pc += n;
    }

    fn fetch(&mut self) -> RealInstruction {
        let stack = self.stack();
        let instr = stack.closure.proto.code[stack.pc as usize];
        self.stack_mut().pc += 1;
        instr
    }

    fn get_const(&mut self, idx: isize) {
        let stack = self.stack_mut();
        let c = &stack.closure.proto.constants[idx as usize];
        let v = LuaValue::from(c.clone());
        stack.push(v);
    }

    fn get_rk(&mut self, rk: isize) {
        if rk > 0xFF {
            // constant
            self.get_const(rk & 0xFF);
        } else {
            // register
            self.push_value(rk + 1);
        }
    }
    fn register_count(&self) -> usize {
        self.stack().closure.proto.max_stack_size as usize
    }

    fn load_vararg(&mut self, mut n: isize) {
        let stack = self.stack_mut();
        if n < 0 {
            n = stack.var_args.len() as isize;
        }

        let varargs = stack.var_args.clone();
        stack.check(n as usize);
        stack.push_n(varargs, n);
    }

    fn load_proto(&mut self, idx: usize) {
        let proto = self.stack().closure.proto.proto_types[idx].clone();
        let closure = LuaValue::Closure(Arc::new(Closure::new(proto)));
        self.stack_mut().push(closure);
    }
}

impl LuaApi for LuaState {
    fn get_top(&self) -> isize {
        self.stack().top()
    }

    fn abs_index(&self, idx: isize) -> usize {
        self.stack().abs_index(idx)
    }

    fn check_stack(&mut self, n: usize) -> bool {
        self.stack_mut().check(n);
        true
    }

    fn pop(&mut self, n: usize) {
        let stack = self.stack_mut();
        for _ in 0..n {
            stack.pop();
        }
    }

    fn copy(&mut self, from_idx: isize, to_idx: isize) {
        let stack = self.stack_mut();
        let val = stack.get(from_idx);
        stack.set(to_idx, val);
    }

    fn push_value(&mut self, idx: isize) {
        let stack = self.stack_mut();
        let val = stack.get(idx);
        stack.push(val);
    }

    fn replace(&mut self, idx: isize) {
        let stack = self.stack_mut();
        let val = stack.pop();
        stack.set(idx, val);
    }

    fn insert(&mut self, idx: isize) {
        self.rotate(idx, 1);
    }

    fn remove(&mut self, idx: isize) {
        self.rotate(idx, -1);
        self.pop(1)
    }

    fn rotate(&mut self, idx: isize, n: isize) {
        let stack = self.stack_mut();
        let t = stack.top() - 1;
        let p = stack.abs_index(idx) - 1;
        let m = if n >= 0 {
            t - n
        } else {
            p as isize - n - 1
        } as usize;
        stack.reverse(p, m);
        stack.reverse(m + 1, t as usize);
        stack.reverse(p, t as usize);
    }

    fn set_top(&mut self, idx: isize) {
        let stack = self.stack_mut();
        stack.set_top(idx);
        // let new_top = stack.abs_index(idx);
        // let old_top = stack.abs_top();
        // if old_top > new_top {
        //     for _ in 0..(old_top - new_top) {
        //         stack.pop();
        //     }
        // } else {
        //     for _ in 0..(new_top - old_top) {
        //         stack.push(LuaValue::Nil);
        //     }
        // }
    }

    fn type_name(&self, tp: LuaValue) -> &'static str {
        tp.name()
    }

    fn get(&self, idx: isize) -> LuaValue {
        self.stack().get(idx)
    }

    fn is_none(&self, idx: isize) -> bool {
        match self.get(idx) {
            LuaValue::None => true,
            _ => false
        }
    }

    fn is_nil(&self, idx: isize) -> bool {
        match self.get(idx) {
            LuaValue::Nil => true,
            _ => false
        }
    }

    fn is_none_or_nil(&self, idx: isize) -> bool {
        self.is_none(idx) || self.is_nil(idx)
    }

    fn is_boolean(&self, idx: isize) -> bool {
        match self.get(idx) {
            LuaValue::Bool(_) => true,
            _ => false
        }
    }

    fn is_integer(&self, idx: isize) -> bool {
        match self.stack().get(idx) {
            LuaValue::Integer(_) => true,
            _ => false,
        }
    }

    fn is_number(&self, idx: isize) -> bool {
        self.to_number_x(idx).is_some()
    }

    fn is_string(&self, idx: isize) -> bool {
        let b = match self.get(idx) {
            LuaValue::String(_) => true,
            _ => false
        };
        b || self.is_number(idx)
    }

    fn is_table(&self, idx: isize) -> bool {
        match self.get(idx) {
            LuaValue::Table(_) => true,
            _ => false
        }
    }

    fn is_thread(&self, idx: isize) -> bool {
        match self.get(idx) {
            LuaValue::Thread => true,
            _ => false
        }
    }

    fn is_function(&self, idx: isize) -> bool {
        match self.get(idx) {
            LuaValue::Function => true,
            _ => false
        }
    }

    fn to_boolean(&self, idx: isize) -> bool {
        self.stack().get(idx).to_boolean()
    }

    fn to_integer(&self, idx: isize) -> i64 {
        self.to_integer_x(idx).unwrap()
    }

    fn to_integer_x(&self, idx: isize) -> Option<i64> {
        match self.stack().get(idx) {
            LuaValue::Integer(i) => Some(i),
            _ => None,
        }
    }

    fn to_number(&self, idx: isize) -> f64 {
        self.to_number_x(idx).unwrap()
    }

    fn to_number_x(&self, idx: isize) -> Option<f64> {
        match self.stack().get(idx) {
            LuaValue::Number(n) => Some(n),
            LuaValue::Integer(i) => Some(i as f64),
            _ => None,
        }
    }

    fn to_string(&self, idx: isize) -> String {
        self.to_string_x(idx).unwrap()
    }

    fn to_string_x(&self, idx: isize) -> Option<String> {
        match self.stack().get(idx) {
            LuaValue::String(s) => Some(s.to_string()),
            LuaValue::Number(n) => Some(n.to_string()),
            LuaValue::Integer(i) => Some(i.to_string()),
            _ => None,
        }
    }

    /* push functions (rust -> stack) */

    fn push_nil(&mut self) {
        self.stack_mut().push(LuaValue::Nil);
    }

    fn push_boolean(&mut self, b: bool) {
        self.stack_mut().push(LuaValue::Bool(b));
    }

    fn push_integer(&mut self, n: i64) {
        self.stack_mut().push(LuaValue::Integer(n));
    }

    fn push_number(&mut self, n: f64) {
        self.stack_mut().push(LuaValue::Number(n));
    }

    fn push_string(&mut self, s: String) {
        self.stack_mut().push(LuaValue::String(Arc::new(s)));
    }

    /* comparison and arithmetic functions */

    fn arith(&mut self, op: ArithOp) {
        let stack = self.stack_mut();
        if op != ArithOp::UNM && op != ArithOp::BNot {
            let b = stack.pop();
            let a = stack.pop();
            if let Some(result) = arith(&a, &b, op) {
                stack.push(result);
                return;
            }
        } else {
            let a = stack.pop();
            if let Some(result) = arith(&a, &a, op) {
                stack.push(result);
                return;
            }
        }
        panic!("arithmetic error!");
    }

    fn compare(&self, idx1: isize, idx2: isize, op: CompareOp) -> bool {
        let stack = self.stack();
        if !stack.is_valid(idx1) || !stack.is_valid(idx2) {
            false
        } else {
            let a = stack.get(idx1);
            let b = stack.get(idx2);
            if let Some(result) = compare(&a, &b, op) {
                return result;
            }
            panic!("comparison error!")
        }
    }

    /* miscellaneous functions */

    fn len(&mut self, idx: isize) {
        let stack = self.stack_mut();
        let l = match stack.get(idx) {
            LuaValue::String(s) => s.len(),
            LuaValue::Table(t) => t.borrow().len(),
            _ => panic!("length error!"),
        };
        stack.push(LuaValue::Integer(l as i64));
    }

    fn concat(&mut self, n: isize) {
        let stack = self.stack_mut();
        if n == 0 {
            stack.push(LuaValue::String(Arc::new(String::new())))
        } else if n >= 2 {
            for _ in 1..n {
                if self.is_string(-1) && self.is_string(-2) {
                    let s2 = self.to_string(-1);
                    let s1 = self.to_string(-2);
                    let mut new_str = String::from(s1);
                    new_str.push_str(&s2);
                    self.stack_mut().pop();
                    self.stack_mut().pop();
                    self.stack_mut().push(LuaValue::String(Arc::new(new_str)));
                } else {
                    panic!("concatenation error!");
                }
            }
        }
        // n == 1, do nothing
    }

    fn new_table(&mut self) {
        self.create_table(0, 0);
    }

    fn create_table(&mut self, arr: usize, rec: usize) {
        let table = LuaTable::new(arr, rec);
        self.stack_mut().push(LuaValue::Table(Arc::new(RefCell::new(table))));
    }

    fn get_table(&mut self, idx: isize) -> LuaValue {
        let stack = self.stack_mut();
        let value = stack.get(idx);
        let key = stack.pop();
        self.get_table_value_by_key(&value, &key)
    }

    fn get_field(&mut self, idx: isize, k: String) -> LuaValue {
        let value = self.stack().get(idx);
        self.get_table_value_by_key(&value,&LuaValue::String(Arc::new(k)))
    }

    fn get_i(&mut self, idx: isize, i: i64) -> LuaValue {
        let value = self.stack().get(idx);
        self.get_table_value_by_key(&value, &LuaValue::Integer(i))
    }

    fn set_table(&mut self, idx: isize) {
        let stack = self.stack_mut();
        let t = stack.get(idx);
        let v = stack.pop();
        let k = stack.pop();
        Self::set_table_kv(&t, k, v);
    }

    fn set_field(&mut self, idx: isize, k: String) {
        let stack = self.stack_mut();
        let t = stack.get(idx);
        let v = stack.pop();
        Self::set_table_kv(&t, LuaValue::String(Arc::new(k)), v);
    }

    fn set_i(&mut self, idx: isize, i: i64) {
        let stack = self.stack_mut();
        let t = stack.get(idx);
        let v = stack.pop();
        Self::set_table_kv(&t, LuaValue::Integer(i), v);
    }

    fn load(&mut self, chunk: Bytes, _chunk_name: &str, _mode: &str) {
        let proto = crate::chunk::un_dump(chunk);
        let c = LuaValue::Closure(Arc::new(Closure::new(proto)));
        self.stack_mut().push(c);
    }

    fn call(&mut self, in_argc: usize, out_argc: isize) {
        let val = self.stack().get(-(in_argc as isize + 1));
        if let LuaValue::Closure(c) = val {
            let source = c.proto.source.clone().unwrap(); // TODO
            let line = c.proto.line_defined;
            let last_line = c.proto.last_line_defined;
            println!("call {}<{},{}>", source, line, last_line);
            self.call_lua_closure(in_argc, out_argc, c);
        } else {
            panic!("not function!");
        }
    }
}



pub(crate) fn print_stack(state: &LuaState) {
    let top = state.get_top();
    for i in 1..=top {
        let t = state.get(i);
        print!("[{}]", t)
    }
    println!();
}

#[cfg(test)]
mod tests {
    use crate::state::{LuaState, print_stack};
    use crate::api::LuaApi;

    #[test]
    fn state() {
        let mut state = LuaState::new();
        state.push_boolean(true);
        print_stack(&state);
        state.push_integer(10);
        print_stack(&state);
        state.push_nil();
        print_stack(&state);
        state.push_string("hello".to_string());
        print_stack(&state);
        state.push_value(-4);
        print_stack(&state);
        state.replace(3);
        print_stack(&state);
        state.set_top(6);
        print_stack(&state);
        state.remove(-3);
        print_stack(&state);
        state.set_top(-5);
        print_stack(&state);
    }
}