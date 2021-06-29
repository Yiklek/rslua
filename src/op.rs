use crate::api::LuaVM;
use crate::arith::{ArithOp, CompareOp, fb2int};
use crate::instruction::Instruction;
use crate::state::upv_index;

/*由于lua指令操作数的长度都没有达到32位，进行类型转换不会出溢出等问题*/


// R(A) := R(B)
pub(crate) fn move_(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, mut b, _) = i.abc();
    a += 1;
    b += 1;

    vm.copy(b as isize, a as isize);
}

// pc+=sBx; if (A) close all upvalues >= R(A - 1)
pub(crate) fn jmp(i: &Instruction, vm: &mut dyn LuaVM) {
    let (a, sbx) = i.a_sbx();

    vm.add_pc(sbx as isize);
    if a != 0 {
        vm.close_upv(a as isize)
    }
}


// R(A), R(A+1), ..., R(A+B) := nil
pub(crate) fn load_nil(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, b, _) = i.abc();
    a += 1;

    vm.push_nil();
    for i in a..(a + b + 1) {
        vm.copy(-1, i as isize);
    }
    vm.pop(1);
}

// R(A) := (bool)B; if (C) pc++
pub(crate) fn load_bool(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, b, c) = i.abc();
    a += 1;

    vm.push_boolean(b != 0);
    vm.replace(a as isize);

    if c != 0 {
        vm.add_pc(1);
    }
}

// R(A) := Kst(Bx)
pub(crate) fn load_k(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, bx) = i.a_bx();
    a += 1;

    vm.get_const(bx as isize);
    vm.replace(a as isize);
}

// R(A) := Kst(extra arg)
pub(crate) fn load_kx(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, _) = i.a_bx();
    a += 1;
    let ax = Instruction(vm.fetch()).ax();

    //vm.CheckStack(1)
    vm.get_const(ax as isize);
    vm.replace(a as isize);
}


/* arith */

// +
pub(crate) fn add(i: &Instruction, vm: &mut dyn LuaVM) {
    binary_arith(i, vm, ArithOp::ADD)
}

// -
pub(crate) fn sub(i: &Instruction, vm: &mut dyn LuaVM) {
    binary_arith(i, vm, ArithOp::SUB)
}

// *
pub(crate) fn mul(i: &Instruction, vm: &mut dyn LuaVM) {
    binary_arith(i, vm, ArithOp::MUL)
}

// %
pub(crate) fn mod_(i: &Instruction, vm: &mut dyn LuaVM) {
    binary_arith(i, vm, ArithOp::MOD)
}

// ^
pub(crate) fn pow(i: &Instruction, vm: &mut dyn LuaVM) {
    binary_arith(i, vm, ArithOp::POW)
}

// /
pub(crate) fn div(i: &Instruction, vm: &mut dyn LuaVM) {
    binary_arith(i, vm, ArithOp::DIV)
}

// //
pub(crate) fn idiv(i: &Instruction, vm: &mut dyn LuaVM) {
    binary_arith(i, vm, ArithOp::IDIV)
}

// &
pub(crate) fn band(i: &Instruction, vm: &mut dyn LuaVM) {
    binary_arith(i, vm, ArithOp::BAnd)
}

// |
pub(crate) fn bor(i: &Instruction, vm: &mut dyn LuaVM) {
    binary_arith(i, vm, ArithOp::BOr)
}

// ~
pub(crate) fn b_xor(i: &Instruction, vm: &mut dyn LuaVM) {
    binary_arith(i, vm, ArithOp::BXor)
}

// <<
pub(crate) fn shl(i: &Instruction, vm: &mut dyn LuaVM) {
    binary_arith(i, vm, ArithOp::SHL)
}

// >>
pub(crate) fn shr(i: &Instruction, vm: &mut dyn LuaVM) {
    binary_arith(i, vm, ArithOp::SHR)
}

// -
pub(crate) fn unm(i: &Instruction, vm: &mut dyn LuaVM) {
    unary_arith(i, vm, ArithOp::UNM)
}

pub(crate) fn b_not(i: &Instruction, vm: &mut dyn LuaVM) {
    unary_arith(i, vm, ArithOp::BNot)
}

// R(A) := RK(B) op RK(C)
fn binary_arith(i: &Instruction, vm: &mut dyn LuaVM, op: ArithOp) {
    let (mut a, b, c) = i.abc();
    a += 1;

    vm.get_rk(b as isize);
    vm.get_rk(c as isize);
    vm.arith(op);
    vm.replace(a as isize);
}

// R(A) := op R(B)
fn unary_arith(i: &Instruction, vm: &mut dyn LuaVM, op: ArithOp) {
    let (mut a, mut b, _) = i.abc();
    a += 1;
    b += 1;

    vm.push_value(b as isize);
    vm.arith(op);
    vm.replace(a as isize);
}

/* compare */

pub(crate) fn eq(i: &Instruction, vm: &mut dyn LuaVM) {
    compare(i, vm, CompareOp::EQ)
}

// ==
pub(crate) fn lt(i: &Instruction, vm: &mut dyn LuaVM) {
    compare(i, vm, CompareOp::LT)
}

// <
pub(crate) fn le(i: &Instruction, vm: &mut dyn LuaVM) {
    compare(i, vm, CompareOp::LE)
} // <=

// if ((RK(B) op RK(C)) ~= A) then pc++
fn compare(i: &Instruction, vm: &mut dyn LuaVM, op: CompareOp) {
    let (a, b, c) = i.abc();

    vm.get_rk(b as isize);
    vm.get_rk(c as isize);
    if vm.compare(-2, -1, op) != (a != 0) {
        vm.add_pc(1);
    }
    vm.pop(2);
}

/* logical */

// R(A) := not R(B)
pub(crate) fn not(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, mut b, _) = i.abc();
    a += 1;
    b += 1;

    vm.push_boolean(!vm.to_boolean(b as isize));
    vm.replace(a as isize);
}

// if not (R(A) <=> C) then pc++
pub(crate) fn test(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, _, c) = i.abc();
    a += 1;

    if vm.to_boolean(a as isize) != (c != 0) {
        vm.add_pc(1);
    }
}

// if (R(B) <=> C) then R(A) := R(B) else pc++
pub(crate) fn test_set(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, mut b, c) = i.abc();
    a += 1;
    b += 1;

    if vm.to_boolean(b as isize) == (c != 0) {
        vm.copy(b as isize, a as isize);
    } else {
        vm.add_pc(1);
    }
}

/* len & concat */

// R(A) := length of R(B)
pub(crate) fn length(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, mut b, _) = i.abc();
    a += 1;
    b += 1;

    vm.len(b as isize);
    vm.replace(a as isize);
}

// R(A) := R(B).. ... ..R(C)
pub(crate) fn concat(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, mut b, mut c) = i.abc();
    a += 1;
    b += 1;
    c += 1;

    let n = c - b + 1;
    vm.check_stack(n as usize);
    for i in b..(c + 1) {
        vm.push_value(i as isize);
    }
    vm.concat(n as isize);
    vm.replace(a as isize);
}

// R(A)-=R(A+2); pc+=sBx
pub(crate) fn for_prep(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, sbx) = i.a_sbx();
    a += 1;

    vm.push_value(a as isize);
    vm.push_value((a + 2) as isize);
    vm.arith(ArithOp::SUB);
    vm.replace(a as isize);
    vm.add_pc(sbx as isize);
}

// R(A)+=R(A+2);
// if R(A) <?= R(A+1) then {
//   pc+=sBx; R(A+3)=R(A)
// }
pub(crate) fn for_loop(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, sbx) = i.a_sbx();
    a += 1;

    // R(A)+=R(A+2);
    vm.push_value((a + 2) as isize);
    vm.push_value(a as isize);
    vm.arith(ArithOp::ADD);
    vm.replace(a as isize);

    let positive_step = vm.to_number((a + 2) as isize).ge(&0.0);
    if positive_step && vm.compare(a as isize, (a + 1) as isize, CompareOp::LE)
        || !positive_step && vm.compare((a + 1) as isize, a as isize, CompareOp::LE) {
        // pc+=sBx; R(A+3)=R(A)
        vm.add_pc(sbx as isize);
        vm.copy(a as isize, (a + 3) as isize);
    }
}


/* number of list items to accumulate before a SETLIST instruction */
const SET_LIST_PER_FLUSH: u32 = 50;

// R(A) := {} (size = B,C)
pub(crate) fn new_table(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, b, c) = i.abc();
    a += 1;

    let arr = fb2int(b as usize);
    let rec = fb2int(c as usize);
    vm.create_table(arr, rec);
    vm.replace(a as isize);
}

// R(A) := R(B)[RK(C)]
pub(crate) fn get_table(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, mut b, c) = i.abc();
    a += 1;
    b += 1;

    vm.get_rk(c as isize);
    vm.get_table(b as isize);
    vm.replace(a as isize);
}

// R(A)[RK(B)] := RK(C)
pub(crate) fn set_table(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, b, c) = i.abc();
    a += 1;

    vm.get_rk(b as isize);
    vm.get_rk(c as isize);
    vm.set_table(a as isize);
}

// R(A)[(C-1)*FPF+i] := R(A+i), 1 <= i <= B
pub(crate) fn set_list(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, mut b, mut c) = i.abc();
    a += 1;

    if c > 0 {
        c = c - 1;
    } else {
        c = Instruction(vm.fetch()).ax();
    }

    let b_is_zero = b == 0;
    if b_is_zero {
        b = (vm.to_integer(-1) - a as i64 - 1) as u32;
        vm.pop(1);
    }
    vm.check_stack(1);
    let mut idx = (c * SET_LIST_PER_FLUSH) as i64;
    for j in 1..=b {
        idx += 1;
        vm.push_value((a + j) as isize);
        vm.set_i(a as isize, idx);
    }
    if b_is_zero {
        let num_reg = vm.register_count() as isize;
        for j in (num_reg + 1)..=vm.get_top() {
            idx += 1;
            vm.push_value(j);
            vm.set_i(a as isize, idx);
        }

        // clear stack
        vm.set_top(num_reg);
    }
}


// R(A) := closure(KPROTO[Bx])
pub(crate) fn closure(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, bx) = i.a_bx();
    a += 1;

    vm.load_proto(bx as usize);
    vm.replace(a as isize);
}

// R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
pub(crate) fn call(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, b, c) = i.abc();
    a += 1;

    // println(":::"+ vm.StackToString())
    let nargs = push_func_and_args(a as isize, b as isize, vm);
    vm.call(nargs, c as isize - 1);
    pop_results(a as isize, c as isize, vm);
}


fn push_func_and_args(a: isize, b: isize, vm: &mut dyn LuaVM) -> usize {
    if b >= 1 {
        vm.check_stack(b as usize);
        for i in a..(a + b) {
            vm.push_value(i);
        }
        b as usize - 1
    } else {
        fix_stack(a, vm);
        vm.get_top() as usize - vm.register_count() - 1
    }
}

fn pop_results(a: isize, c: isize, vm: &mut dyn LuaVM) {
    if c == 1 {
        // no results
    } else if c > 1 {
        for i in (a..(a + c - 1)).rev() {
            vm.replace(i);
        }
    } else {
        // leave results on stack
        vm.check_stack(1);
        vm.push_integer(a as i64);
    }
}

fn fix_stack(a: isize, vm: &mut dyn LuaVM) {
    let x = vm.to_integer(-1) as isize;
    vm.pop(1);

    vm.check_stack((x - a) as usize);
    for i in a..x {
        vm.push_value(i);
    }
    vm.rotate(vm.register_count() as isize + 1, x - a);
}

// return R(A), ... ,R(A+B-2)
pub(crate) fn return_(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, b, _) = i.abc();
    a += 1;

    if b == 1 {
        // no return values
    } else if b > 1 {
        // b-1 return values
        vm.check_stack(b as usize - 1);
        for i in a..(a + b - 1) {
            vm.push_value(i as isize);
        }
    } else {
        fix_stack(a as isize, vm);
    }
}

// R(A), R(A+1), ..., R(A+B-2) = vararg
pub(crate) fn vararg(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, b, _) = i.abc();
    a += 1;

    if b != 1 {
        // b==0 or b>1
        vm.load_vararg(b as isize - 1);
        pop_results(a as isize, b as isize, vm)
    }
}


// return R(A)(R(A+1), ... ,R(A+B-1))
pub(crate) fn tail_call(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, b, _) = i.abc();
    a += 1;

    // todo: optimize tail call!
    let c = 0;
    let nargs = push_func_and_args(a as isize, b as isize, vm);
    vm.call(nargs, c - 1);
    pop_results(a as isize, c, vm);
}

// R(A+1) := R(B); R(A) := R(B)[RK(C)]
pub(crate) fn self_(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, mut b, c) = i.abc();
    a += 1;
    b += 1;

    vm.copy(b as isize, (a + 1) as isize);
    vm.get_rk(c as isize);
    vm.get_table(b as isize);
    vm.replace(a as isize);
}

// R(A) := UpValue[B][RK(C)]
pub(crate) fn get_tab_up(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, mut b, c) = i.abc();
    a += 1;
    b += 1;

    vm.get_rk(c as isize);
    vm.get_table(upv_index(b as isize));
    vm.replace(a as isize);
}

// UpValue[A][RK(B)] := R(C)
pub(crate) fn set_tab_up(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, mut b, c) = i.abc();
    a += 1;

    vm.get_rk(b as isize);
    vm.get_rk(c as isize);
    vm.set_table(upv_index(a as isize))
}

// R(A) := UpValue[B]
pub(crate) fn get_up_val(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, mut b, _) = i.abc();
    a += 1;
    b += 1;
    vm.copy(upv_index(b as isize), a as isize);
}

// UpValue[B] := R(A)
pub(crate) fn set_up_val(i: &Instruction, vm: &mut dyn LuaVM) {
    let (mut a, mut b, _) = i.abc();
    a += 1;
    b += 1;
    vm.copy(a as isize, upv_index(b as isize));
}
