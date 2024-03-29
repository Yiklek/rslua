use std::sync::Arc;
use crate::chunk::{Constant, ProtoType};
use crate::api::LuaValue::Integer;
use std::fmt::{Display, Formatter, Debug};
use crate::arith::{ArithOp, CompareOp, random};
use std::cell::RefCell;
use crate::vm::PCAddr;
use crate::instruction::RealInstruction;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use crate::{arith};
use bytes::Bytes;
use std::ops::Deref;

pub trait LuaApi {
    /* basic stack manipulation */
    fn get_top(&self) -> isize;
    fn abs_index(&self, idx: isize) -> usize;
    fn check_stack(&mut self, n: usize) -> bool;
    fn pop(&mut self, n: usize);
    fn copy(&mut self, from_idx: isize, to_idx: isize);
    fn push_value(&mut self, idx: isize);
    fn replace(&mut self, idx: isize);
    fn insert(&mut self, idx: isize);
    fn remove(&mut self, idx: isize);
    fn rotate(&mut self, idx: isize, n: isize);
    fn set_top(&mut self, idx: isize);
    /* access functions (stack -> rust) */
    fn type_name(&self, tp: LuaValue) -> &'static str;
    fn get(&self, idx: isize) -> LuaValue;
    // `type` is a keyword
    fn is_none(&self, idx: isize) -> bool;
    fn is_nil(&self, idx: isize) -> bool;
    fn is_none_or_nil(&self, idx: isize) -> bool;
    fn is_boolean(&self, idx: isize) -> bool;
    fn is_integer(&self, idx: isize) -> bool;
    fn is_number(&self, idx: isize) -> bool;
    fn is_string(&self, idx: isize) -> bool;
    fn is_table(&self, idx: isize) -> bool;
    fn is_thread(&self, idx: isize) -> bool;
    fn is_function(&self, idx: isize) -> bool;
    fn is_rust_function(&self, idx: isize) -> bool;
    fn to_boolean(&self, idx: isize) -> bool;
    fn to_integer(&self, idx: isize) -> i64;
    fn to_integer_x(&self, idx: isize) -> Option<i64>;
    fn to_number(&self, idx: isize) -> f64;
    fn to_number_x(&self, idx: isize) -> Option<f64>;
    fn to_string(&self, idx: isize) -> String;
    fn to_string_x(&self, idx: isize) -> Option<String>;
    fn to_rust_function(&self, idx: isize) -> Option<RustFn>;
    /* push functions (rust -> stack) */
    fn push_nil(&mut self);
    fn push_boolean(&mut self, b: bool);
    fn push_integer(&mut self, n: i64);
    fn push_number(&mut self, n: f64);
    fn push_string(&mut self, s: String);
    fn push_rust_function(&mut self, f: RustFn);
    fn push_global_table(&mut self);
    /* comparison and arithmetic functions */
    fn arith(&mut self, op: ArithOp);
    fn compare(&self, idx1: isize, idx2: isize, op: CompareOp) -> bool;
    /* miscellaneous functions */
    fn len(&mut self, idx: isize);
    fn concat(&mut self, n: isize);
    /* get functions (Lua -> stack) */
    fn new_table(&mut self);
    fn create_table(&mut self, arr: usize, rec: usize);
    fn get_table(&mut self, idx: isize) -> LuaValue;
    fn get_field(&mut self, idx: isize, k: String) -> LuaValue;
    fn get_i(&mut self, idx: isize, i: i64) -> LuaValue;
    fn get_global(&mut self, name: String) -> LuaValue;
    /* set functions (stack -> Lua) */
    fn set_table(&mut self, idx: isize);
    fn set_field(&mut self, idx: isize, k: String);
    fn set_i(&mut self, idx: isize, i: i64);
    fn set_global(&mut self, name: String);
    fn register(&mut self, name: String, f: RustFn);
    /*function*/
    fn load(&mut self, chunk: Bytes, chunk_name: &str, mode: &str);
    fn call(&mut self, in_argc: usize, out_argc: isize);
}

pub trait LuaVM: LuaApi {
    fn pc(&self) -> PCAddr;
    fn add_pc(&mut self, n: isize);
    fn fetch(&mut self) -> RealInstruction;
    fn get_const(&mut self, idx: isize);
    fn get_rk(&mut self, rk: isize);
    fn register_count(&self) -> usize;
    fn load_vararg(&mut self, n: isize);
    fn load_proto(&mut self, idx: usize);
}

#[derive(Clone, Debug)]
pub struct LuaTable {
    arr: Vec<LuaValue>,
    map: HashMap<LuaValue, LuaValue>,
    rdm: usize, // hash code
}

impl Hash for LuaTable {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.rdm.hash(state);
    }
}

impl LuaTable {
    pub fn new(arr: usize, rec: usize) -> LuaTable {
        LuaTable {
            arr: Vec::with_capacity(arr),
            map: HashMap::with_capacity(rec),
            rdm: arith::random(),
        }
    }

    pub fn len(&self) -> usize {
        self.arr.len()
    }

    pub fn get(&self, key: &LuaValue) -> LuaValue {
        if let Some(idx) = to_index(key) {
            if idx <= self.arr.len() {
                return self.arr[idx - 1].clone(); // TODO
            }
        }
        if let Some(val) = self.map.get(key) {
            val.clone() // TODO
        } else {
            LuaValue::Nil
        }
    }

    pub fn put(&mut self, key: LuaValue, val: LuaValue) {
        if key.is_nil() {
            panic!("table index is nil!");
        }
        if let LuaValue::Number(n) = key {
            if n.is_nan() {
                panic!("table index is NaN!");
            }
        }

        if let Some(idx) = to_index(&key) {
            let arr_len = self.arr.len();
            if idx <= arr_len {
                let val_is_nil = val.is_nil();
                self.arr[idx - 1] = val;
                if idx == arr_len && val_is_nil {
                    self.shrink_array();
                }
                return;
            }
            if idx == arr_len + 1 {
                self.map.remove(&key);
                if !val.is_nil() {
                    self.arr.push(val);
                    self.expand_array();
                }
                return;
            }
        }

        if !val.is_nil() {
            self.map.insert(key, val);
        } else {
            self.map.remove(&key);
        }
    }

    fn shrink_array(&mut self) {
        while !self.arr.is_empty() {
            if self.arr.last().unwrap().is_nil() {
                self.arr.pop();
            } else {
                break;
            }
        }
    }

    fn expand_array(&mut self) {
        let mut idx = self.arr.len() + 1;
        loop {
            let key = LuaValue::Integer(idx as i64);
            if self.map.contains_key(&key) {
                let val = self.map.remove(&key).unwrap();
                self.arr.push(val);
                idx += 1;
            } else {
                break;
            }
        }
    }
}

fn to_index(key: &LuaValue) -> Option<usize> {
    if let LuaValue::Integer(i) = key {
        if *i >= 1 {
            return Some(*i as usize);
        }
    } else if let LuaValue::Number(n) = key {
        if let Some(i) = float_to_integer(*n) {
            if i >= 1 {
                return Some(i as usize);
            }
        }
    }
    None
}

#[derive(Clone)]
pub struct Closure {
    pub(crate) proto: Arc<ProtoType>,
    pub(crate) rust_fn: Option<RustFn>,
    rdm: usize,
}

impl Debug for Closure {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Closure")
            .field("proto", &*self.proto)
            .field("rust_fn", &"rust_fn")
            .field("rdm", &self.rdm)
            .finish()
    }
}

//todo derive hash
impl Hash for Closure {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.rdm.hash(state);
    }
}

impl Closure {
    pub fn with_proto(proto: Arc<ProtoType>) -> Closure {
        Closure {
            proto,
            rust_fn: None,
            rdm: random(),
        }
    }
    pub fn with_func(f: RustFn) -> Closure {
        Closure {
            proto: Arc::new(ProtoType::new_fake()),
            rust_fn: Some(f),
            rdm: random(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum LuaValue {
    None,
    Nil,
    Bool(bool),
    LightUserData,
    Number(f64),
    Integer(i64),
    String(Arc<String>),
    Table(Arc<RefCell<LuaTable>>),
    Function,
    Closure(Arc<Closure>),
    UserData,
    Thread,
}

impl PartialEq for LuaValue {
    fn eq(&self, other: &LuaValue) -> bool {
        if let (LuaValue::Nil, LuaValue::Nil) = (self, other) {
            true
        } else if let (LuaValue::Bool(x), LuaValue::Bool(y)) = (self, other) {
            x == y
        } else if let (LuaValue::Integer(x), LuaValue::Integer(y)) = (self, other) {
            x == y
        } else if let (LuaValue::Number(x), LuaValue::Number(y)) = (self, other) {
            x == y // TODO
        } else if let (LuaValue::String(x), LuaValue::String(y)) = (self, other) {
            x == y
        } else if let (LuaValue::Table(x), LuaValue::Table(y)) = (self, other) {
            Arc::ptr_eq(x, y)
        } else {
            false
        }
    }
}

// the trait `std::cmp::Eq` is not implemented for `f64`
impl Eq for LuaValue {} // TODO

// the trait `std::hash::Hash` is not implemented for `f64`
impl Hash for LuaValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            LuaValue::Nil => 0.hash(state),
            LuaValue::Bool(b) => b.hash(state),
            LuaValue::Integer(i) => i.hash(state),
            LuaValue::Number(n) => n.to_bits().hash(state),
            LuaValue::String(s) => s.hash(state),
            LuaValue::Table(t) => t.borrow().hash(state),
            LuaValue::Closure(c) => c.hash(state),
            _ => {}
        }
    }
}

impl Display for LuaValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            LuaValue::None => "none".to_string(),
            LuaValue::Nil => "nil".to_string(),
            LuaValue::Bool(b) => b.to_string(),
            LuaValue::LightUserData => "LightUserData".to_string(),
            LuaValue::Number(f) => f.to_string(),
            LuaValue::Integer(i) => i.to_string(),
            LuaValue::String(s) => format!("\"{}\"", s),
            LuaValue::Table(_) => "Table".to_string(),
            LuaValue::Function => "Function".to_string(),
            LuaValue::Closure(_) => "Function".to_string(),
            LuaValue::UserData => "UserData".to_string(),
            LuaValue::Thread => "Thread".to_string(),
        };
        f.write_str(&s)
    }
}

impl LuaValue {
    pub fn name(&self) -> &'static str {
        match self {
            LuaValue::None => "no value",
            LuaValue::Nil => "nil",
            LuaValue::Bool(_) => "bool",
            LuaValue::LightUserData => "LightUserData",
            LuaValue::Number(_) => "number",
            LuaValue::Integer(_) => "integer",
            LuaValue::String(_) => "string",
            LuaValue::Table(_) => "table",
            LuaValue::Function => "function",
            LuaValue::Closure(_) => "function",
            LuaValue::UserData => "userdata",
            LuaValue::Thread => "thread",
        }
    }
    pub fn is_nil(&self) -> bool {
        match self {
            LuaValue::Nil => true,
            _ => false,
        }
    }
    pub fn to_boolean(&self) -> bool {
        match self {
            LuaValue::Nil => false,
            LuaValue::Bool(b) => *b, // TODO
            _ => true,
        }
    }

    // http://www.lua.org/manual/5.3/manual.html#3.4.3
    pub fn to_number(&self) -> Option<f64> {
        match self {
            LuaValue::Integer(i) => Some(*i as f64),
            LuaValue::Number(n) => Some(*n),
            LuaValue::String(s) => s.parse::<f64>().ok(), // TODO
            _ => None,
        }
    }

    // http://www.lua.org/manual/5.3/manual.html#3.4.3
    pub fn to_integer(&self) -> Option<i64> {
        match self {
            LuaValue::Integer(i) => Some(*i),
            LuaValue::Number(n) => float_to_integer(*n),
            LuaValue::String(s) => string_to_integer(&*s),
            _ => None,
        }
    }
}

fn string_to_integer(s: &String) -> Option<i64> {
    if let Ok(i) = s.parse::<i64>() {
        Some(i)
    } else if let Ok(n) = s.parse::<f64>() {
        float_to_integer(n)
    } else {
        None
    }
}

pub fn float_to_integer(n: f64) -> Option<i64> {
    let i = n as i64;
    if i as f64 == n {
        Some(i)
    } else {
        None
    }
}

impl From<Constant> for LuaValue {
    fn from(c: Constant) -> Self {
        match c {
            Constant::Integer(i) => Integer(i),
            Constant::String(s) => LuaValue::String(Arc::new(s)),
            Constant::Nil => LuaValue::Nil,
            Constant::Boolean(b) => LuaValue::Bool(b),
            Constant::Number(f) => LuaValue::Number(f)
        }
    }
}

// pub type RustFn = Arc<Box<dyn FnMut(&mut dyn LuaVM) -> usize>>;
pub type RustFn = fn(&mut dyn LuaVM) -> usize;

#[cfg(test)]
mod tests {
    use crate::vm::lua_main;
    use crate::chunk::print_chunk;

    #[test]
    fn table() {
        let path = "tests/table.out";
        print_chunk(&path);
        lua_main(&path);
    }
}