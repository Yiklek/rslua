use std::rc::Rc;
use bytes::{Bytes, Buf};
use crate::instruction::{RealInstruction, Instruction, OpMode, OpArgMode};
use std::io::Read;

mod constants {
    pub const LUA_SIGNATURE: [u8; 4] = [0x1b, 0x4c, 0x75, 0x61];
    // "\x1bLua"
    pub const LUA_C_VERSION: u8 = 0x53;
    pub const LUA_C_FORMAT: u8 = 0;
    pub const LUA_C_DATA: [u8; 6] = [0x19, 0x93, 0x0d, 0x0a, 0x1a, 0x0a];
    // "\x19\x93\r\n\x1a\n"
    pub const C_INT_SIZE: u8 = 4;
    pub const C_SIZE_T_SIZE: u8 = 8;
    pub const INSTRUCTION_SIZE: u8 = 4;
    pub const LUA_INTEGER_SIZE: u8 = 8;
    pub const LUA_NUMBER_SIZE: u8 = 8;
    pub const LUA_C_INT: i64 = 0x5678;
    pub const LUA_C_NUM: f64 = 370.5;

    pub const TAG_NIL: u8 = 0x00;
    pub const TAG_BOOLEAN: u8 = 0x01;
    pub const TAG_NUMBER: u8 = 0x03;
    pub const TAG_INTEGER: u8 = 0x13;
    pub const TAG_SHORT_STR: u8 = 0x04;
    pub const TAG_LONG_STR: u8 = 0x14;
}

#[derive(Debug)]
struct Header {
    signature: [u8; 4],
    version: u8,
    format: u8,
    lua_c_data: [u8; 6],
    c_int_size: u8,
    c_size_t_size: u8,
    instruction_size: u8,
    lua_integer_size: u8,
    lua_number_size: u8,
    lua_c_int: i64,
    lua_c_num: f64,
}

#[derive(Debug)]
struct Chunk {
    header: Header,
    // ?
    size_up_values: u8,
    main_func: ProtoType,
}

#[derive(Debug)]
pub struct ProtoType {
    pub source: Option<String>,
    // debug
    pub line_defined: u32,
    pub last_line_defined: u32,
    //fixed parameters
    pub num_params: u8,
    pub is_vararg: u8,
    pub max_stack_size: u8,
    pub code: Vec<RealInstruction>,
    pub constants: Vec<Constant>,
    pub up_values: Vec<UpValue>,
    pub proto_types: Vec<Rc<ProtoType>>,
    // debug
    pub line_info: Vec<u32>,
    // debug
    pub loc_vars: Vec<LocVar>,
    pub up_value_names: Vec<String>, // debug
}

impl ProtoType {
    pub(crate) fn new_fake() -> Self {
        Self {
            source: None,
            // debug
            line_defined: 0,
            last_line_defined: 0,
            num_params: 0,
            is_vararg: 0,
            max_stack_size: 0,
            code: vec![],
            constants: vec![],
            up_values: vec![],
            proto_types: vec![],
            // debug
            line_info: vec![],
            // debug
            loc_vars: vec![],
            up_value_names: vec![], // debug
        }
    }
}


#[derive(Debug)]
pub struct UpValue {
    pub in_stack: u8,
    pub idx: u8,
}

#[derive(Debug)]
pub struct LocVar {
    pub var_name: String,
    pub start_pc: u32,
    pub end_pc: u32,
}

#[derive(Debug, Clone)]
pub enum Constant {
    Nil,
    Boolean(bool),
    Number(f64),
    Integer(i64),
    String(String),
}

pub(crate) fn un_dump(bs: Bytes) -> Rc<ProtoType> {
    let mut reader = ChunkReader::new(bs);
    reader.check_header();
    reader.read_byte();
    reader.read_proto()
}

pub(crate) fn un_dump_file(file_path: &dyn ToString) -> Rc<ProtoType> {
    let mut file = std::fs::File::open(file_path.to_string()).unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data).unwrap();
    let bytes = Bytes::from(data);
    un_dump(bytes)
}
pub(crate) fn read_file(file_path: &dyn ToString) -> Bytes {
    let mut file = std::fs::File::open(file_path.to_string()).unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data).unwrap();
    let bytes = Bytes::from(data);
    bytes
}

#[derive(Debug)]
struct ChunkReader {
    data: Bytes
}

impl ChunkReader {
    pub fn new(data: Bytes) -> Self {
        Self { data }
    }

    pub fn read_byte(&mut self) -> u8 {
        self.data.get_u8()
    }

    fn read_u32(&mut self) -> u32 {
        self.data.get_u32_le()
    }

    fn read_u64(&mut self) -> u64 {
        self.data.get_u64_le()
    }

    fn read_lua_integer(&mut self) -> i64 {
        self.read_u64() as i64
    }

    fn read_lua_number(&mut self) -> f64 {
        self.data.get_f64_le()
    }

    fn read_bytes(&mut self, n: usize) -> Vec<u8> {
        let mut vec = Vec::new();
        for _ in 0..n {
            vec.push(self.read_byte());
        }
        vec
    }

    fn read_string(&mut self) -> String {
        self.read_string0().unwrap_or_else(|| String::new())
    }

    fn read_string0(&mut self) -> Option<String> {
        let mut size = self.read_byte() as usize;
        if size == 0 {
            return None;
        }
        if size == 0xFF {
            size = self.read_u64() as usize; // size_t
        }
        let bytes = self.read_bytes(size - 1);
        let string = String::from_utf8(bytes);
        string.ok() // Some(string.unwrap())
    }

    fn read_vec<T, F>(&mut self, f: F) -> Vec<T>
        where
            F: Fn(&mut ChunkReader) -> T,
    {
        let n = self.read_u32() as usize;
        let mut vec = Vec::with_capacity(n);
        for _i in 0..n {
            vec.push(f(self));
        }
        vec
    }

    pub fn check_header(&mut self) {
        assert_eq!(self.read_bytes(4), constants::LUA_SIGNATURE, "not a precompiled chunk!");
        assert_eq!(self.read_byte(), constants::LUA_C_VERSION, "version mismatch!");
        assert_eq!(self.read_byte(), constants::LUA_C_FORMAT, "format mismatch!");
        assert_eq!(self.read_bytes(6), constants::LUA_C_DATA, "corrupted!");
        assert_eq!(self.read_byte(), constants::C_INT_SIZE, "int size mismatch!");
        assert_eq!(self.read_byte(), constants::C_SIZE_T_SIZE, "size_t size mismatch!");
        assert_eq!(self.read_byte(), constants::INSTRUCTION_SIZE, "instruction size mismatch!");
        assert_eq!(self.read_byte(), constants::LUA_INTEGER_SIZE, "lua_Integer size mismatch!");
        assert_eq!(self.read_byte(), constants::LUA_NUMBER_SIZE, "lua_Number size mismatch!");
        assert_eq!(self.read_lua_integer(), constants::LUA_C_INT, "endianness mismatch!");
        assert_eq!(self.read_lua_number(), constants::LUA_C_NUM, "float format mismatch!");
    }

    pub fn read_proto(&mut self) -> Rc<ProtoType> {
        self.read_proto0(None)
    }

    fn read_proto0(&mut self, parent_source: Option<String>) -> Rc<ProtoType> {
        let source = self.read_string0().or(parent_source);
        Rc::new(ProtoType {
            source: source.clone(), // debug
            line_defined: self.read_u32(),
            last_line_defined: self.read_u32(),
            num_params: self.read_byte(),
            is_vararg: self.read_byte(),
            max_stack_size: self.read_byte(),
            code: self.read_vec(|r| r.read_u32() as RealInstruction),
            constants: self.read_vec(|r| r.read_constant()),
            up_values: self.read_vec(|r| r.read_up_value()),
            proto_types: self.read_vec(|r| r.read_proto0(source.clone())),
            line_info: self.read_vec(|r| r.read_u32()),        // debug
            loc_vars: self.read_vec(|r| r.read_loc_var()),     // debug
            up_value_names: self.read_vec(|r| r.read_string()), // debug
        })
    }

    fn read_constant(&mut self) -> Constant {
        let tag = self.read_byte();
        match tag {
            constants::TAG_NIL => Constant::Nil,
            constants::TAG_BOOLEAN => Constant::Boolean(self.read_byte() != 0),
            constants::TAG_INTEGER => Constant::Integer(self.read_lua_integer()),
            constants::TAG_NUMBER => Constant::Number(self.read_lua_number()),
            constants::TAG_SHORT_STR => Constant::String(self.read_string()),
            constants::TAG_LONG_STR => Constant::String(self.read_string()),
            _ => panic!("corrupted!"),
        }
    }

    fn read_up_value(&mut self) -> UpValue {
        UpValue {
            in_stack: self.read_byte(),
            idx: self.read_byte(),
        }
    }

    fn read_loc_var(&mut self) -> LocVar {
        LocVar {
            var_name: self.read_string(),
            start_pc: self.read_u32(),
            end_pc: self.read_u32(),
        }
    }
}

fn list(p: &ProtoType) {
    print_header(p);
    print_code(p);
    print_detail(p);
    for i in &p.proto_types {
        list(&*i);
    }
}

fn print_header(p: &ProtoType) {
    let mut func_type = "main";
    if p.line_defined > 0 {
        func_type = "function";
    }
    let mut var_arg_flag = "";
    if p.is_vararg > 0 {
        var_arg_flag = "+";
    }
    print!("\n{} <{:?}:{},{}> ({} instruction)\n",
           func_type, p.source.as_ref().unwrap_or(&"".to_string()), p.line_defined, p.last_line_defined, p.code.len());
    print!("{}{} params, {} slots, {} upvalues, ",
           p.num_params, var_arg_flag, p.max_stack_size, p.up_values.len());
    print!("{} locals, {} constants, {} functions\n",
           p.loc_vars.len(), p.constants.len(), p.proto_types.len());
}

fn print_code(p: &ProtoType) {
    for i in 0..p.code.len() {
        let mut line = "-".to_string();
        if p.line_info.len() > 0 {
            line = format!("{}", p.line_info[i]);
        }
        let inst = Instruction(p.code[i]);
        print!("\t{}\t[{}]\t{} \t", i + 1, line, inst.op_name());
        print_operands(inst);
        println!();
    }
}

fn print_operands(i: Instruction) {
    match i.op_mode() {
        OpMode::ABC => {
            let (a, b, c) = i.abc();
            print!("a:{}", a);
            if i.b_mode() != OpArgMode::N {
                if b > 0xff {
                    print!(" b:{}", -1isize - (b as isize & 0xff));
                } else {
                    print!(" b:{}", b);
                }
            }
            if i.c_mode() != OpArgMode::N {
                if c > 0xff {
                    print!(" c:{}", -1isize - (c as isize & 0xff));
                } else {
                    print!(" c:{}", c);
                }
            }
        }
        OpMode::ABx => {
            let (a, bx) = i.a_bx();
            print!("a:{}", a);
            if i.b_mode() == OpArgMode::K {
                print!(" bx:{}", -1isize - bx as isize);
            } else if i.b_mode() == OpArgMode::U {
                print!(" bx:{}", bx);
            }
        }
        OpMode::AsBx => {
            let (a, sbx) = i.a_sbx();
            print!("a:{} sbx:{}", a, sbx);
        }
        OpMode::Ax => {
            let ax = i.ax();
            print!("ax:{}", ax);
        }
    }
}

fn print_detail(p: &ProtoType) {
    print!("constants ({}):\n", p.constants.len());
    for i in 0..p.constants.len() {
        print!("\t{}\t{:?}\n", i + 1, p.constants[i]);
    }

    print!("locals ({}):\n", p.loc_vars.len());
    for i in 0..p.loc_vars.len() {
        let var = &p.loc_vars[i];
        print!("\t{}\t{}\t{}\t{}\n", i, var.var_name, var.start_pc + 1, var.end_pc + 1);
    }

    print!("upvalues ({}):\n", p.up_values.len());
    for i in 0..p.up_values.len() {
        let value = &p.up_values[i];
        print!("\t{}\t{}\t{}\t{}\n", i, p.up_value_names[i], value.in_stack, value.idx);
    }
}

pub fn print_chunk(path: &dyn ToString) {
    let mut file = std::fs::File::open(path.to_string()).unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data).unwrap();
    let bytes = Bytes::from(data);
    let arc = un_dump(bytes);
    let a = arc;
    list(&*a);
}
#[cfg(test)]
mod tests {
    use crate::chunk::{un_dump, list, print_chunk};

    #[test]
    fn read_chunk() {
        print_chunk(&"tests/table.out");
    }
}
