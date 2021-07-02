use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use bytes::Bytes;

use crate::api::{Closure, LuaApi, LuaTable, LuaValue, LuaVM, RustFn, SharedLuaValue};
use crate::arith::{arith, ArithOp, compare, CompareOp};
use crate::chunk::ProtoType;
use crate::instruction::{Instruction, RawOp, RealInstruction};
use crate::vm::PCAddr;

#[derive(Debug)]
pub(crate) struct LuaStack {
    slots: Vec<SharedLuaValue>,
    // slots
    closure: Closure,
    var_args: Vec<SharedLuaValue>,
    pc: PCAddr,
    registry: SharedLuaValue,
    open_upv: HashMap<isize, SharedLuaValue>
}


impl LuaStack {
    pub fn new(size: usize, registry: SharedLuaValue, closure: Closure) -> LuaStack {
        LuaStack {
            slots: Vec::with_capacity(size),
            registry,
            closure,
            var_args: vec![],
            pc: 0,
            open_upv: HashMap::new()
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

    pub fn push(&mut self, val: SharedLuaValue) {
        self.slots.push(val);
    }

    pub fn pop(&mut self) -> SharedLuaValue {
        self.slots.pop().unwrap()
    }

    pub fn pop_n(&mut self, n: usize) -> Vec<SharedLuaValue> {
        let mut vec = Vec::with_capacity(n);
        for _ in 0..n {
            vec.push(self.pop());
        }
        vec.reverse();
        vec
    }

    pub fn push_n(&mut self, mut vals: Vec<SharedLuaValue>, n: isize) {
        vals.reverse();
        let val_len = vals.len();
        let un = if n < 0 { val_len } else { n as usize };

        for i in 0..un {
            if i < val_len {
                self.push(vals.pop().unwrap());
            } else {
                self.push(SharedLuaValue::from(LuaValue::Nil));
            }
        }
    }

    pub fn set_top(&mut self, idx: isize) {
        let new_top = self.abs_index(idx);
        let old_top = self.abs_top();
        if old_top > new_top {
            for _ in 0..(old_top - new_top) {
                self.pop();
            }
        } else {
            for _ in 0..(new_top - old_top) {
                self.push(SharedLuaValue::from(LuaValue::Nil));
            }
        }
    }

    pub fn abs_index(&self, idx: isize) -> usize {
        if idx >= 0 || idx <= LUA_REGISTRY_INDEX {
            idx as usize
        } else {
            (idx + self.top() + 1) as usize
        }
    }

    pub fn is_valid(&self, idx: isize) -> bool {
        if idx < LUA_REGISTRY_INDEX {
            let uv_idx = LUA_REGISTRY_INDEX - idx - 1;
            return uv_idx < self.closure.up_values.len() as isize
        } else if idx == LUA_REGISTRY_INDEX { return true; }
        let abs_idx = self.abs_index(idx);
        abs_idx > 0 && abs_idx <= self.abs_top()
    }

    pub fn get(&self, idx: isize) -> SharedLuaValue {
        if idx < LUA_REGISTRY_INDEX {
            let uv_idx = LUA_REGISTRY_INDEX - idx - 1;
            return if uv_idx >= self.closure.up_values.len() as isize {
                LuaValue::Nil.into()
            } else {
                self.closure.up_values[uv_idx as usize].clone()
            }
        } else if idx == LUA_REGISTRY_INDEX { return self.registry.clone(); }
        let abs_idx = self.abs_index(idx);
        if abs_idx > 0 && abs_idx <= self.abs_top() {
            let idx = abs_idx - 1;
            let a = &self.slots[idx];
            a.clone()
        } else {
            SharedLuaValue::from(LuaValue::Nil)
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

    pub fn set(&mut self, idx: isize, val: SharedLuaValue) {
        if idx < LUA_REGISTRY_INDEX {
            let uv_idx = LUA_REGISTRY_INDEX - idx - 1;
            if uv_idx < self.closure.up_values.len() as isize {
                self.closure.up_values[uv_idx as usize].set_val(val)
            }
            return;
        } else if idx == LUA_REGISTRY_INDEX {
            self.registry.set_val(val);
            return;
        }
        let abs_idx = self.abs_index(idx);
        if abs_idx > 0 && abs_idx <= self.abs_top() {
            let idx = abs_idx as usize - 1;
            self.slots[idx].set_rc(val)
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
    registry: SharedLuaValue,
}

const LUA_MIN_STACK: usize = 20;
const LUA_MAX_STACK: usize = 1000000;
const LUA_REGISTRY_INDEX: isize = -(LUA_MAX_STACK as isize) - 1000;
const LUA_R_IDX_GLOBALS: i64 = 2;

pub(crate) fn upv_index(i: isize) -> isize {
    LUA_REGISTRY_INDEX - i
}

impl LuaState {
    pub fn new() -> LuaState {
        Self::with_capacity(LUA_MIN_STACK)
    }
    pub fn with_capacity(i: usize) -> LuaState {
        let mut table = LuaTable::new(0, 0);
        table.put(SharedLuaValue::from(LuaValue::Integer(LUA_R_IDX_GLOBALS)),
                  SharedLuaValue::from(LuaValue::Table(LuaTable::new(0, 0))));
        let registry = LuaValue::Table(table);
        let registry = SharedLuaValue::from(registry);
        let stacks = vec![LuaStack::new(i, registry.clone(),
                                        Closure::with_proto(Rc::new(ProtoType::new_fake())))];
        LuaState { stacks, registry }
    }
    fn stack_mut(&mut self) -> &mut LuaStack {
        self.stacks.last_mut().expect("must be at least 1 stack.")
    }

    pub(crate) fn stack(&self) -> &LuaStack {
        self.stacks.last().expect("must be at least 1 stack.")
    }

    fn push_stack(&mut self, frame: LuaStack) {
        self.stacks.push(frame);
    }

    fn pop_stack(&mut self) -> LuaStack {
        self.stacks.pop().unwrap()
    }


    fn get_table_value_by_key(&mut self, t: &SharedLuaValue, k: &SharedLuaValue) -> SharedLuaValue {
        if let LuaValue::Table(tbl) = t.borrow().deref() {
            let v = tbl.get(k);
            self.stack_mut().push(v.clone());
            v
        } else {
            panic!("not a table!") // todo
        }
    }

    fn set_table_kv(t: &mut SharedLuaValue, k: SharedLuaValue, v: SharedLuaValue) {
        if let LuaValue::Table(tbl) = t.borrow_mut().deref_mut() {
            tbl.put(k, v);
        } else {
            panic!("not a table!");
        }
    }
    fn call_rust_closure(&mut self, in_argc: usize, out_argc: isize, c: Closure) {
        // create new lua stack
        let rust_fn = c.rust_fn.expect("rust closure can't be none");
        let mut new_stack = LuaStack::new(in_argc + LUA_MIN_STACK, self.registry.clone(), c);

        // pass args, pop func
        if in_argc > 0 {
            let args = self.stack_mut().pop_n(in_argc);
            new_stack.push_n(args, in_argc as isize);
        }
        self.stack_mut().pop(); // pop func

        // run closure
        self.push_stack(new_stack);
        let r = rust_fn(self);
        new_stack = self.pop_stack();

        // return results
        if out_argc != 0 {
            let results = new_stack.pop_n(r);
            self.stack_mut().check(results.len());
            self.stack_mut().push_n(results, out_argc);
        }
    }

    fn call_lua_closure(&mut self, in_argc: usize, out_argc: isize, c: Closure) {
        let num_regs = c.proto.max_stack_size as usize;
        let num_params = c.proto.num_params as usize;
        let is_vararg = c.proto.is_vararg == 1;
        // create new lua stack
        let mut new_stack = LuaStack::new(num_regs + LUA_MIN_STACK, self.registry.clone(), c);

        let stack = self.stack_mut();
        // pass args, pop func
        let mut args = stack.pop_n(in_argc);
        stack.pop(); // pop func
        if in_argc > num_params {
            //more args
            for _i in num_params..in_argc {
                new_stack.var_args.push(args.pop().unwrap());
            }
            //  varargs
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
        stack.push(SharedLuaValue::from(v));
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
        let mut closure = Closure::with_proto(proto);
        for (i, upv) in closure.proto.up_values.iter().enumerate() {
            if upv.in_stack == 1 {
                if self.stack().open_upv.contains_key(&(upv.idx as isize)) {
                    // if self.stack().open_upv.contains(&(upv.idx as isize)){
                    closure.up_values[i].set_rc(self.stack().get(upv.idx as isize + 1));
                } else {
                    let value = self.stack().get(upv.idx as isize + 1);
                    closure.up_values[i].set_rc(value.clone());
                    self.stack_mut().open_upv.insert(upv.idx as isize, value);
                }
            } else {
                closure.up_values[i].set_rc(self.stack().closure.up_values[upv.idx as usize].clone());
            }
        }
        let closure = LuaValue::Closure(closure);
        self.stack_mut().push(SharedLuaValue::from(closure));
    }
    fn close_upv(&mut self, idx: isize) {}
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
    }

    fn type_name(&self, tp: SharedLuaValue) -> &'static str {
        tp.borrow().name()
    }

    fn get(&self, idx: isize) -> SharedLuaValue {
        self.stack().get(idx)
    }

    fn is_none(&self, idx: isize) -> bool {
        match &*self.get(idx).borrow() {
            LuaValue::None => true,
            _ => false
        }
    }

    fn is_nil(&self, idx: isize) -> bool {
        match &*self.get(idx).borrow() {
            LuaValue::Nil => true,
            _ => false
        }
    }

    fn is_none_or_nil(&self, idx: isize) -> bool {
        self.is_none(idx) || self.is_nil(idx)
    }

    fn is_boolean(&self, idx: isize) -> bool {
        match &*self.get(idx).borrow() {
            LuaValue::Bool(_) => true,
            _ => false
        }
    }

    fn is_integer(&self, idx: isize) -> bool {
        match &*self.stack().get(idx).borrow() {
            LuaValue::Integer(_) => true,
            _ => false,
        }
    }

    fn is_number(&self, idx: isize) -> bool {
        self.to_number_x(idx).is_some()
    }

    fn is_string(&self, idx: isize) -> bool {
        let b = match &*self.get(idx).borrow() {
            LuaValue::String(_) => true,
            _ => false
        };
        b || self.is_number(idx)
    }

    fn is_table(&self, idx: isize) -> bool {
        match &*self.get(idx).borrow() {
            LuaValue::Table(_) => true,
            _ => false
        }
    }

    fn is_thread(&self, idx: isize) -> bool {
        match &*self.get(idx).borrow() {
            LuaValue::Thread => true,
            _ => false
        }
    }

    fn is_function(&self, idx: isize) -> bool {
        match &*self.get(idx).borrow() {
            LuaValue::Function => true,
            LuaValue::Closure(_) => true,
            _ => false
        }
    }

    fn is_rust_function(&self, idx: isize) -> bool {
        match &*self.stack().get(idx).borrow() {
            LuaValue::Closure(c) => c.rust_fn.is_some(),
            _ => false,
        }
    }

    fn to_boolean(&self, idx: isize) -> bool {
        self.stack().get(idx).borrow().to_boolean()
    }

    fn to_integer(&self, idx: isize) -> i64 {
        self.to_integer_x(idx).unwrap()
    }

    fn to_integer_x(&self, idx: isize) -> Option<i64> {
        match &*self.stack().get(idx).borrow() {
            LuaValue::Integer(i) => Some(*i),
            _ => None,
        }
    }

    fn to_number(&self, idx: isize) -> f64 {
        self.to_number_x(idx).unwrap()
    }

    fn to_number_x(&self, idx: isize) -> Option<f64> {
        match &*self.stack().get(idx).borrow() {
            LuaValue::Number(n) => Some(*n),
            LuaValue::Integer(i) => Some(*i as f64),
            _ => None,
        }
    }

    fn to_string(&self, idx: isize) -> String {
        self.to_string_x(idx).unwrap()
    }

    fn to_string_x(&self, idx: isize) -> Option<String> {
        match self.stack().get(idx).borrow().deref() {
            LuaValue::String(s) => Some(s.to_string()),
            LuaValue::Number(n) => Some(n.to_string()),
            LuaValue::Integer(i) => Some(i.to_string()),
            _ => None,
        }
    }

    fn to_rust_function(&self, idx: isize) -> Option<RustFn> {
        match self.stack().get(idx).borrow().deref() {
            LuaValue::Closure(c) => c.rust_fn.clone(),
            _ => None,
        }
    }

    /* push functions (rust -> stack) */

    fn push_nil(&mut self) {
        self.stack_mut().push(LuaValue::Nil.into());
    }

    fn push_boolean(&mut self, b: bool) {
        self.stack_mut().push(LuaValue::Bool(b).into());
    }

    fn push_integer(&mut self, n: i64) {
        self.stack_mut().push(LuaValue::Integer(n).into());
    }

    fn push_number(&mut self, n: f64) {
        self.stack_mut().push(LuaValue::Number(n).into());
    }

    fn push_string(&mut self, s: String) {
        self.stack_mut().push(LuaValue::String(s).into());
    }

    fn push_rust_function(&mut self, f: RustFn) {
        self.stack_mut().push(LuaValue::Closure(Closure::with_func(f, 0)).into());
    }

    fn push_rust_closure(&mut self, f: RustFn, upv: usize) {
        let pops = self.stack_mut().pop_n(upv);
        self.stack_mut().push(LuaValue::Closure(Closure::with_func_upv(f, pops)).into());
    }

    fn push_global_table(&mut self) {
        let mut global = None;
        if let LuaValue::Table(t) = self.registry.borrow().deref() {
            global = Some(t.get(&LuaValue::Integer(LUA_R_IDX_GLOBALS).into()));
        };
        if let Some(g) = global {
            self.stack_mut().push(g);
        }
    }

    /* comparison and arithmetic functions */

    fn arith(&mut self, op: ArithOp) {
        let stack = self.stack_mut();
        match op {
            ArithOp::UNM | ArithOp::BNot => {
                let a = stack.pop();
                if let Some(result) = arith(&*a.borrow(), &*a.borrow(), op) {
                    stack.push(result.into());
                    return;
                };
            }
            _ => {
                let b = stack.pop();
                let a = stack.pop();
                if let Some(result) = arith(&*a.borrow(), &*b.borrow(), op) {
                    stack.push(result.into());
                    return;
                };
            }
        }
    }

    fn compare(&self, idx1: isize, idx2: isize, op: CompareOp) -> bool {
        let stack = self.stack();
        if !stack.is_valid(idx1) || !stack.is_valid(idx2) {
            false
        } else {
            let a = stack.get(idx1);
            let b = stack.get(idx2);
            if let Some(result) = compare(&*a.borrow(), &*b.borrow(), op) {
                return result;
            }
            panic!("comparison error!")
        }
    }

    /* miscellaneous functions */

    fn len(&mut self, idx: isize) {
        let stack = self.stack_mut();
        let l = match stack.get(idx).borrow().deref() {
            LuaValue::String(s) => s.len(),
            LuaValue::Table(t) => t.len(),
            _ => panic!("length error!"),
        };
        stack.push(LuaValue::Integer(l as i64).into());
    }

    fn concat(&mut self, n: isize) {
        let stack = self.stack_mut();
        if n == 0 {
            stack.push(LuaValue::String(String::new()).into())
        } else if n >= 2 {
            for _ in 1..n {
                if self.is_string(-1) && self.is_string(-2) {
                    let s2 = self.to_string(-1);
                    let s1 = self.to_string(-2);
                    let mut new_str = String::from(s1);
                    new_str.push_str(&s2);
                    self.stack_mut().pop();
                    self.stack_mut().pop();
                    self.stack_mut().push(LuaValue::String(new_str).into());
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
        self.stack_mut().push(LuaValue::Table(table).into());
    }

    fn get_table(&mut self, idx: isize) -> SharedLuaValue {
        let stack = self.stack_mut();
        let value = stack.get(idx);
        let key = stack.pop();
        self.get_table_value_by_key(&value, &key)
    }

    fn get_field(&mut self, idx: isize, k: String) -> SharedLuaValue {
        let value = self.stack().get(idx);
        self.get_table_value_by_key(&value, &LuaValue::String(k).into())
    }

    fn get_i(&mut self, idx: isize, i: i64) -> SharedLuaValue {
        let value = self.stack().get(idx);
        self.get_table_value_by_key(&value, &LuaValue::Integer(i).into())
    }

    fn get_global(&mut self, name: String) -> SharedLuaValue {
        let value = self.registry.borrow().clone();
        if let LuaValue::Table(r) = value {
            let t = r.get(&LuaValue::Integer(LUA_R_IDX_GLOBALS).into());
            let k = LuaValue::String(name);
            self.get_table_value_by_key(&t, &k.into())
        } else {
            LuaValue::None.into()
        }
    }

    fn set_table(&mut self, idx: isize) {
        let stack = self.stack_mut();
        let mut t = stack.get(idx);
        let v = stack.pop();
        let k = stack.pop();
        Self::set_table_kv(&mut t, k, v);
    }

    fn set_field(&mut self, idx: isize, k: String) {
        let stack = self.stack_mut();
        let mut t = stack.get(idx);
        let v = stack.pop();
        Self::set_table_kv(&mut t, LuaValue::String(k).into(), v);
    }

    fn set_i(&mut self, idx: isize, i: i64) {
        let stack = self.stack_mut();
        let mut t = stack.get(idx);
        let v = stack.pop();
        Self::set_table_kv(&mut t, LuaValue::Integer(i).into(), v);
    }

    fn set_global(&mut self, name: String) {
        let reg = self.registry.clone();
        let value = reg.borrow();
        if let LuaValue::Table(r) = value.deref() {
            let mut t = r.get(&LuaValue::Integer(LUA_R_IDX_GLOBALS).into());
            let v = self.stack_mut().pop();
            let k = LuaValue::String(name);
            Self::set_table_kv(&mut t, k.into(), v);
        }
    }

    fn register(&mut self, name: String, f: RustFn) {
        self.push_rust_function(f);
        self.set_global(name);
    }

    fn load(&mut self, chunk: Bytes, _chunk_name: &str, _mode: &str) {
        let proto = crate::chunk::un_dump(chunk);
        let mut closure = Closure::with_proto(proto);
        if closure.proto.up_values.len() > 0 {
            if let LuaValue::Table(t) = &*self.registry.borrow() {
                let env = t.get(&LuaValue::Integer(LUA_R_IDX_GLOBALS).into());
                closure.up_values[0] = env;
            }
        }
        let c = LuaValue::Closure(closure);
        self.stack_mut().push(c.into());
    }

    fn call(&mut self, in_argc: usize, out_argc: isize) {
        let val = self.stack().get(-(in_argc as isize + 1));
        if let LuaValue::Closure(c) = val.borrow().deref() {
            if c.rust_fn.is_some() {
                self.call_rust_closure(in_argc, out_argc, c.clone());
            } else {
                // let source = c.proto.source.clone().unwrap_or_else(|| "none".to_string());
                // let line = c.proto.line_defined;
                // let last_line = c.proto.last_line_defined;
                // println!("call {}<{},{}>", source, line, last_line);
                self.call_lua_closure(in_argc, out_argc, c.clone());
            }
        } else {
            panic!("not function! {:?}", self.stack());
        };
    }
}


pub(crate) fn print_stack(state: &LuaState) {
    let top = state.get_top();
    for i in 1..=top {
        let t = state.get(i);
        print!("[{}]", t);
    };
    println!();
}

pub(crate) fn format_stack(state: &LuaState) -> String {
    let mut r = String::new();
    let top = state.get_top();
    for i in 1..=top {
        let t = state.get(i);
        let string = format!("[{}]", t);
        r.push_str(string.as_str());
    };
    r
}

#[cfg(test)]
mod tests {
    use crate::api::LuaApi;
    use crate::state::{format_stack, LuaState, print_stack};

    #[test]
    fn state() {
        let mut state = LuaState::new();
        state.push_boolean(true);
        assert_eq!(format_stack(&state), "[true]");
        print_stack(&state);

        state.push_integer(10);
        assert_eq!(format_stack(&state), "[true][10]");
        print_stack(&state);

        state.push_nil();
        assert_eq!(format_stack(&state), "[true][10][nil]");
        print_stack(&state);

        state.push_string("hello".to_string());
        assert_eq!(format_stack(&state), "[true][10][nil][\"hello\"]");
        print_stack(&state);

        state.push_value(-4);
        assert_eq!(format_stack(&state), "[true][10][nil][\"hello\"][true]");
        print_stack(&state);

        state.replace(3);
        assert_eq!(format_stack(&state), "[true][10][true][\"hello\"]");
        print_stack(&state);

        state.set_top(6);
        assert_eq!(format_stack(&state), "[true][10][true][\"hello\"][nil][nil]");
        print_stack(&state);

        state.remove(-3);
        assert_eq!(format_stack(&state), "[true][10][true][nil][nil]");
        print_stack(&state);

        state.set_top(-5);
        assert_eq!(format_stack(&state), "[true]");
        print_stack(&state);
    }
}
