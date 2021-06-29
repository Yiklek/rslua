use crate::{impl_enum_from, op};
use crate::api::LuaVM;
use crate::state::LuaState;

/*
 31       22       13       5    0
  +-------+^------+-^-----+-^-----
  |b=9bits |c=9bits |a=8bits|op=6|   ABC
  +-------+^------+-^-----+-^-----
  |    bx=18bits    |a=8bits|op=6|   ABx
  +-------+^------+-^-----+-^-----
  |   sbx=18bits    |a=8bits|op=6|   AsBx
  +-------+^------+-^-----+-^-----
  |    ax=26bits            |op=6|   Ax
  +-------+^------+-^-----+-^-----
 31      23      15       7      0
*/
#[derive(Copy, Clone)]
pub enum OpMode {
    ABC,
    ABx,
    AsBx,
    Ax,
}

#[repr(u8)]
#[derive(Copy, Clone,Eq, PartialEq)]
#[allow(dead_code)]
pub(crate) enum RawOp {
    MOVE = 0,
    LOADK,
    LOADKX,
    LOADBOOL,
    LOADNIL,
    GETUPVAL,
    GETTABUP,
    GETTABLE,
    SETTABUP,
    SETUPVAL,
    SETTABLE,
    NEWTABLE,
    SELF,
    ADD,
    SUB,
    MUL,
    MOD,
    POW,
    DIV,
    IDIV,
    BAND,
    BOR,
    BXOR,
    SHL,
    SHR,
    UNM,
    BNOT,
    NOT,
    LEN,
    CONCAT,
    JMP,
    EQ,
    LT,
    LE,
    TEST,
    TESTSET,
    CALL,
    TAILCALL,
    RETURN,
    FORLOOP,
    FORPREP,
    TFORCALL,
    TFORLOOP,
    SETLIST,
    CLOSURE,
    VARARG,
    EXTRAARG,
}

#[derive(Copy, Clone,Eq, PartialEq)]
pub enum OpArgMode {
    N,
    U,
    R,
    K,
}
impl_enum_from!(u8, RawOp);

static OPCODES: once_cell::sync::Lazy<[OpCode; 47]> = once_cell::sync::Lazy::new(|| {
    [
        OpCode::new(0, 1, OpArgMode::R, OpArgMode::N, OpMode::ABC, "MOVE    ", op::move_), // R(A) := R(B)
        OpCode::new(0, 1, OpArgMode::K, OpArgMode::N, OpMode::ABx, "LOADK   ", op::load_k), // R(A) := Kst(Bx)
        OpCode::new(0, 1, OpArgMode::N, OpArgMode::N, OpMode::ABx, "LOADKX  ", op::load_kx), // R(A) := Kst(extra arg)
        OpCode::new(0, 1, OpArgMode::U, OpArgMode::U, OpMode::ABC, "LOADBOOL", op::load_bool), // R(A) := (bool)B; if (C) pc++
        OpCode::new(0, 1, OpArgMode::U, OpArgMode::N, OpMode::ABC, "LOADNIL ", op::load_nil), // R(A), R(A+1), ..., R(A+B) := nil
        OpCode::new(0, 1, OpArgMode::U, OpArgMode::N, OpMode::ABC, "GETUPVAL", op::get_up_val), // R(A) := UpValue[B]
        OpCode::new(0, 1, OpArgMode::U, OpArgMode::K, OpMode::ABC, "GETTABUP", op::get_tab_up), // R(A) := UpValue[B][RK(C)]
        OpCode::new(0, 1, OpArgMode::R, OpArgMode::K, OpMode::ABC, "GETTABLE", op::get_table), // R(A) := R(B)[RK(C)]
        OpCode::new(0, 1, OpArgMode::K, OpArgMode::K, OpMode::ABC, "SETTABUP", op::set_tab_up), // UpValue[A][RK(B)] := RK(C)
        OpCode::new(0, 1, OpArgMode::U, OpArgMode::N, OpMode::ABC, "SETUPVAL", op::set_up_val), // UpValue[B] := R(A)
        OpCode::new(0, 1, OpArgMode::K, OpArgMode::K, OpMode::ABC, "SETTABLE", op::set_table), // R(A)[RK(B)] := RK(C)
        OpCode::new(0, 1, OpArgMode::U, OpArgMode::U, OpMode::ABC, "NEWTABLE", op::new_table), // R(A) := {} (size = B,C)
        OpCode::new(0, 1, OpArgMode::R, OpArgMode::K, OpMode::ABC, "SELF    ", op::self_), // R(A+1) := R(B); R(A) := R(B)[RK(C)]
        OpCode::new(0, 1, OpArgMode::K, OpArgMode::K, OpMode::ABC, "ADD     ", op::add), // R(A) := RK(B) + RK(C)
        OpCode::new(0, 1, OpArgMode::K, OpArgMode::K, OpMode::ABC, "SUB     ", op::sub), // R(A) := RK(B) - RK(C)
        OpCode::new(0, 1, OpArgMode::K, OpArgMode::K, OpMode::ABC, "MUL     ", op::mul), // R(A) := RK(B) * RK(C)
        OpCode::new(0, 1, OpArgMode::K, OpArgMode::K, OpMode::ABC, "MOD     ", op::mod_), // R(A) := RK(B) % RK(C)
        OpCode::new(0, 1, OpArgMode::K, OpArgMode::K, OpMode::ABC, "POW     ", op::pow), // R(A) := RK(B) ^ RK(C)
        OpCode::new(0, 1, OpArgMode::K, OpArgMode::K, OpMode::ABC, "DIV     ", op::div), // R(A) := RK(B) / RK(C)
        OpCode::new(0, 1, OpArgMode::K, OpArgMode::K, OpMode::ABC, "IDIV    ", op::idiv), // R(A) := RK(B) // RK(C)
        OpCode::new(0, 1, OpArgMode::K, OpArgMode::K, OpMode::ABC, "BAND    ", op::band), // R(A) := RK(B) & RK(C)
        OpCode::new(0, 1, OpArgMode::K, OpArgMode::K, OpMode::ABC, "BOR     ", op::bor), // R(A) := RK(B) | RK(C)
        OpCode::new(0, 1, OpArgMode::K, OpArgMode::K, OpMode::ABC, "BXOR    ", op::b_xor), // R(A) := RK(B) ~ RK(C)
        OpCode::new(0, 1, OpArgMode::K, OpArgMode::K, OpMode::ABC, "SHL     ", op::shl), // R(A) := RK(B) << RK(C)
        OpCode::new(0, 1, OpArgMode::K, OpArgMode::K, OpMode::ABC, "SHR     ", op::shr), // R(A) := RK(B) >> RK(C)
        OpCode::new(0, 1, OpArgMode::R, OpArgMode::N, OpMode::ABC, "UNM     ", op::unm), // R(A) := -R(B)
        OpCode::new(0, 1, OpArgMode::R, OpArgMode::N, OpMode::ABC, "BNOT    ", op::b_not), // R(A) := ~R(B)
        OpCode::new(0, 1, OpArgMode::R, OpArgMode::N, OpMode::ABC, "NOT     ", op::not), // R(A) := not R(B)
        OpCode::new(0, 1, OpArgMode::R, OpArgMode::N, OpMode::ABC, "LEN     ", op::length), // R(A) := length of R(B)
        OpCode::new(0, 1, OpArgMode::R, OpArgMode::R, OpMode::ABC, "CONCAT  ", op::concat), // R(A) := R(B).. ... ..R(C)
        OpCode::new(0, 1, OpArgMode::R, OpArgMode::N, OpMode::AsBx, "JMP     ", op::jmp), // pc+=sBx; if (A) close all upvalues >= R(A - 1)
        OpCode::new(0, 1, OpArgMode::K, OpArgMode::K, OpMode::ABC, "EQ      ", op::eq), // if ((RK(B) == RK(C)) ~= A) then pc++
        OpCode::new(0, 1, OpArgMode::K, OpArgMode::K, OpMode::ABC, "LT      ", op::lt), // if ((RK(B) <  RK(C)) ~= A) then pc++
        OpCode::new(0, 1, OpArgMode::K, OpArgMode::K, OpMode::ABC, "LE      ", op::le), // if ((RK(B) <= RK(C)) ~= A) then pc++
        OpCode::new(0, 1, OpArgMode::N, OpArgMode::U, OpMode::ABC, "TEST    ", op::test), // if not (R(A) <=> C) then pc++
        OpCode::new(0, 1, OpArgMode::R, OpArgMode::U, OpMode::ABC, "TESTSET ", op::test_set), // if (R(B) <=> C) then R(A) := R(B) else pc++
        OpCode::new(0, 1, OpArgMode::U, OpArgMode::U, OpMode::ABC, "CALL    ", op::call), // R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
        OpCode::new(0, 1, OpArgMode::U, OpArgMode::U, OpMode::ABC, "TAILCALL", op::tail_call), // return R(A)(R(A+1), ... ,R(A+B-1))
        OpCode::new(0, 1, OpArgMode::U, OpArgMode::N, OpMode::ABC, "RETURN  ", op::return_), // return R(A), ... ,R(A+B-2)
        OpCode::new(0, 1, OpArgMode::R, OpArgMode::N, OpMode::AsBx, "FORLOOP ", op::for_loop), // R(A)+=R(A+2); if R(A) <?= R(A+1) then { pc+=sBx; R(A+3)=R(A) }
        OpCode::new(0, 1, OpArgMode::R, OpArgMode::N, OpMode::AsBx, "FORPREP ", op::for_prep), // R(A)-=R(A+2); pc+=sBx
        OpCode::new(0, 1, OpArgMode::N, OpArgMode::U, OpMode::ABC, "TFORCALL", UNIMPLEMENTED_ACTION), // R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2));
        OpCode::new(0, 1, OpArgMode::R, OpArgMode::N, OpMode::AsBx, "TFORLOOP", UNIMPLEMENTED_ACTION), // if R(A+1) ~= nil then { R(A)=R(A+1); pc += sBx }
        OpCode::new(0, 1, OpArgMode::U, OpArgMode::U, OpMode::ABC, "SETLIST ", op::set_list), // R(A)[(C-1)*FPF+i] := R(A+i), 1 <= i <= B
        OpCode::new(0, 1, OpArgMode::U, OpArgMode::N, OpMode::ABx, "CLOSURE ", op::closure), // R(A) := closure(KPROTO[Bx])
        OpCode::new(0, 1, OpArgMode::U, OpArgMode::N, OpMode::ABC, "VARARG  ", op::vararg), // R(A), R(A+1), ..., R(A+B-2) = vararg
        OpCode::new(0, 1, OpArgMode::U, OpArgMode::U, OpMode::Ax, "EXTRAARG", UNIMPLEMENTED_ACTION), // extra (larger) argument for previous opcode
    ]
});

pub type RealInstruction = u32;
type UnSignedOperand = u32;
type SignedOperand = i32;
type OpAction = fn(&Instruction, &mut dyn LuaVM);

const OP_CODE_LEN :usize = 6;
const ABC_A_LEN :usize = 8;
#[allow(dead_code)]
const ABC_B_LEN :usize = 9;
const ABC_C_LEN :usize = 9;
const ABX_A_LEN :usize = 8;
const ABX_BX_LEN :usize = 18;
#[allow(dead_code)]
const A_SBX_A_LEN :usize = 8;
#[allow(dead_code)]
const A_SBX_SBX_LEN :usize = 18;
#[allow(dead_code)]
const AX_AX_LEN :usize = 26;
const MAX_ARG_BX: usize = (1 << ABX_BX_LEN) - 1;
const MAX_ARG_SBX: usize = MAX_ARG_BX >> 1;

const UNIMPLEMENTED_ACTION: OpAction = |_, _| { unimplemented!() };

#[derive(Copy, Clone)]
pub struct Instruction(pub(crate) RealInstruction);

impl Instruction{
    pub fn op_name(&self) -> &'static str {
        self.op_code().name
    }

    pub fn op_mode(&self) -> OpMode {
        self.op_code().op_mode
    }

    pub fn b_mode(&self) -> OpArgMode {
        self.op_code().arg_b_mode
    }

    pub fn c_mode(&self) -> OpArgMode {
        self.op_code().arg_c_mode
    }

    pub(crate) fn raw_op_code(&self) -> RawOp {
        RawOp::from(self.0 as u8 & 0x3F)
    }
    pub(crate) fn op_code(&self) -> OpCode{
        OPCODES[self.raw_op_code() as usize]
    }

    pub fn abc(&self) -> (UnSignedOperand, UnSignedOperand, UnSignedOperand) {
        let a = (self.0 >> OP_CODE_LEN & 0xFF) as UnSignedOperand;
        let c = (self.0 >> (OP_CODE_LEN + ABC_A_LEN) & 0x1FF) as UnSignedOperand;
        let b = (self.0 >> (OP_CODE_LEN + ABC_A_LEN + ABC_C_LEN) & 0x1FF) as UnSignedOperand;
        (a, b, c)
    }

    pub fn a_bx(&self) -> (UnSignedOperand, UnSignedOperand) {
        let a = (self.0 >> OP_CODE_LEN & 0xFF) as UnSignedOperand;
        let bx = (self.0 >> (OP_CODE_LEN + ABX_A_LEN)) as UnSignedOperand;
        (a, bx)
    }

    pub fn a_sbx(&self) -> (UnSignedOperand, SignedOperand) {
        let (a, bx) = self.a_bx();
        (a, bx as SignedOperand - MAX_ARG_SBX as SignedOperand)
    }

    pub fn ax(&self) -> UnSignedOperand {
        (self.0 >> OP_CODE_LEN) as UnSignedOperand
    }
    pub(crate) fn execute(&self, vm: &mut LuaState) {
        let action = self.op_code().action;
        action(self, vm);
    }
}

#[derive(Copy, Clone)]
pub(crate)struct OpCode {
    test_flag: u8,
    set_a_flag: u8,
    arg_b_mode: OpArgMode,
    arg_c_mode: OpArgMode,
    op_mode: OpMode,
    name: &'static str,
    action: OpAction,
}

impl OpCode {
    fn new(
        test_flag: u8,
        set_a_flag: u8,
        arg_b_mode: OpArgMode,
        arg_c_mode: OpArgMode,
        op_mode: OpMode,
        name: &'static str,
        action: OpAction,
    ) -> OpCode {
        OpCode {
            test_flag,
            set_a_flag,
            arg_b_mode,
            arg_c_mode,
            op_mode,
            name,
            action,
        }
    }
}

