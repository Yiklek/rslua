use crate::api::LuaValue;
use crate::impl_enum_from;

fn floor_div_i(a: i64, b: i64) -> i64 {
    if a > 0 && b > 0 || a < 0 && b < 0 || a % b == 0 {
        a / b
    } else {
        a / b - 1
    }
}

pub fn floor_div_f(a: f64, b: f64) -> f64 {
    (a / b).floor()
}

// a % b == a - ((a // b) * b)
pub fn mod_i(a: i64, b: i64) -> i64 {
    a - floor_div_i(a, b) * b
}

// a % b == a - ((a // b) * b)
pub fn mod_f(a: f64, b: f64) -> f64 {
    if a > 0.0 && is_positive_infinite(b) || a < 0.0 && is_negative_infinite(b) {
        a
    } else if a > 0.0 && is_negative_infinite(b) || a < 0.0 && is_positive_infinite(b) {
        b
    } else {
        a - (a / b).floor() * b
    }
}

fn is_positive_infinite(n: f64) -> bool {
    n.is_infinite() && n.is_sign_positive()
}

fn is_negative_infinite(n: f64) -> bool {
    n.is_infinite() && n.is_sign_negative()
}


pub fn shift_left(a: i64, n: i64) -> i64 {
    if n >= 64 {
        0
    } else if n >= 0 {
        a << n
    } else {
        shift_right(a, -n)
    }
}

// logical shift right
pub fn shift_right(a: i64, n: i64) -> i64 {
    if n >= 64 {
        0
    } else if n >= 0 {
        (a as u64 >> n) as i64
    } else {
        shift_left(a, -n)
    }
}

fn add_i(a: i64, b: i64) -> i64 {
    a + b
}

fn add_f(a: f64, b: f64) -> f64 {
    a + b
}

fn sub_i(a: i64, b: i64) -> i64 {
    a - b
}

fn sub_f(a: f64, b: f64) -> f64 {
    a - b
}

fn mul_i(a: i64, b: i64) -> i64 {
    a * b
}

fn mul_f(a: f64, b: f64) -> f64 {
    a * b
}

// fn imod(a: i64, b: i64) -> i64 {
//     super::math::i_mod(a, b)
// }
// fn fmod(a: f64, b: f64) -> f64 {
//     super::math::f_mod(a, b)
// }
fn pow_f(a: f64, b: f64) -> f64 {
    a.powf(b)
}

fn div_f(a: f64, b: f64) -> f64 {
    a / b
}

// fn iidiv(a: i64, b: i64) -> i64 {
//     super::math::i_floor_div(a, b)
// }
// fn fidiv(a: f64, b: f64) -> f64 {
//     super::math::f_floor_div(a, b)
// }
fn and_b(a: i64, b: i64) -> i64 {
    a & b
}

fn or_b(a: i64, b: i64) -> i64 {
    a | b
}

fn xor_b(a: i64, b: i64) -> i64 {
    a ^ b
}

fn unm_i(a: i64, _: i64) -> i64 {
    -a
}

fn unm_f(a: f64, _: f64) -> f64 {
    -a
}

fn not_b(a: i64, _: i64) -> i64 {
    !a
}

fn none_i(_: i64, _: i64) -> i64 {
    0
}

fn none_f(_: f64, _: f64) -> f64 {
    0.0
}

pub const OPS: &'static [(fn(i64, i64) -> i64, fn(f64, f64) -> f64)] = &[
    (add_i, add_f),
    (sub_i, sub_f),
    (mul_i, mul_f),
    (mod_i, mod_f),
    (none_i, pow_f),
    (none_i, div_f),
    (floor_div_i, floor_div_f),
    (and_b, none_f),
    (or_b, none_f),
    (xor_b, none_f),
    (shift_left, none_f),
    (shift_right, none_f),
    (unm_i, unm_f),
    (not_b, none_f),
];

#[repr(u8)]
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum ArithOp {
    ADD = 0,
    // +
    SUB,
    // -
    MUL,
    // *
    MOD,
    // %
    POW,
    // ^
    DIV,
    // /
    IDIV,
    // //
    BAnd,
    // &
    BOr,
    // |
    BXor,
    // ~
    SHL,
    // <<
    SHR,
    // >>
    UNM,
    // -
    BNot, //
}
impl_enum_from!(u8, ArithOp);
pub(crate) fn arith(a: &LuaValue, b: &LuaValue, op: ArithOp) -> Option<LuaValue> {
    let iop = OPS[op as usize].0;
    let fop = OPS[op as usize].1;
    if fop == none_f {
        // bitwise
        if let Some(x) = a.to_integer() {
            if let Some(y) = b.to_integer() {
                return Some(LuaValue::Integer(iop(x, y)));
            }
        }
    } else {
        // arith
        if iop != none_i {
            // add,sub,mul,mod,idiv,unm
            if let LuaValue::Integer(x) = a {
                if let LuaValue::Integer(y) = b {
                    return Some(LuaValue::Integer(iop(*x, *y)));
                }
            }
        }
        if let Some(x) = a.to_number() {
            if let Some(y) = b.to_number() {
                return Some(LuaValue::Number(fop(x, y)));
            }
        }
    }
    None
}

#[repr(u8)]
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum CompareOp {
    EQ,
    // ==
    LT,
    // <
    LE, // <=
}
impl_enum_from!(u8,CompareOp);



pub(crate) fn compare(a: &LuaValue, b: &LuaValue, op: CompareOp) -> Option<bool> {
    match op {
        CompareOp::EQ => Some(eq(a, b)),
        CompareOp::LT => lt(a, b),
        CompareOp::LE => le(a, b),
    }
}

macro_rules! cmp {
    ($a:ident $op:tt $b:ident) => {
        match $a {
            LuaValue::String(x) => match $b {
                LuaValue::String(y) => Some(x $op y),
                _ => None,
            },
            LuaValue::Integer(x) => match $b {
                LuaValue::Integer(y) => Some(x $op y),
                LuaValue::Number(y) => Some((*x as f64) $op *y),
                _ => None,
            },
            LuaValue::Number(x) => match $b {
                LuaValue::Number(y) => Some(x $op y),
                LuaValue::Integer(y) => Some(*x $op (*y as f64)),
                _ => None,
            },
            _ => None,
        }
    }
}

fn eq(a: &LuaValue, b: &LuaValue) -> bool {
    if let Some(x) = cmp!(a == b) {
        x
    } else {
        match a {
            LuaValue::Nil => match b {
                LuaValue::Nil => true,
                _ => false,
            },
            LuaValue::Bool(x) => match b {
                LuaValue::Bool(y) => x == y,
                _ => false,
            },
            _ => false,
        }
    }
}

fn lt(a: &LuaValue, b: &LuaValue) -> Option<bool> {
    cmp!(a < b)
}

fn le(a: &LuaValue, b: &LuaValue) -> Option<bool> {
    cmp!(a <= b)
}




#[allow(dead_code)]
pub fn int2fb(mut x: usize) -> usize {
    let mut e = 0; /* exponent */
    if x < 8 {
        return x;
    }
    while x >= (8 << 4) {
        /* coarse steps */
        x = (x + 0xf) >> 4; /* x = ceil(x / 16) */
        e += 4;
    }
    while x >= (8 << 1) {
        /* fine steps */
        x = (x + 1) >> 1; /* x = ceil(x / 2) */
        e += 1;
    }
    return ((e + 1) << 3) | (x - 8);
}

/* converts back */
pub fn fb2int(x: usize) -> usize {
    if x < 8 {
        x
    } else {
        ((x & 7) + 8) << ((x >> 3) - 1)
    }
}

// https://users.rust-lang.org/t/random-number-without-using-the-external-crate/17260/8
pub fn random() -> usize {
    let ptr = Box::into_raw(Box::new(123));
    ptr as usize
}
#[cfg(test)]
mod tests {
    use crate::state::{LuaState, print_stack, format_stack};
    use crate::api::LuaApi;
    use crate::arith::{ArithOp, CompareOp};

    #[test]
    fn arith() {
        let mut state = LuaState::new();
        state.push_integer(1);
        state.push_string("2.0".to_string());
        state.push_string("3.0".to_string());
        state.push_number(4.0);
        assert_eq!(format_stack(&state),"[1][\"2.0\"][\"3.0\"][4]");
        print_stack(&state);

        state.arith(ArithOp::ADD);
        assert_eq!(format_stack(&state),"[1][\"2.0\"][7]");
        print_stack(&state);

        state.arith(ArithOp::BNot);
        assert_eq!(format_stack(&state),"[1][\"2.0\"][-8]");
        print_stack(&state);

        state.len(2);
        assert_eq!(format_stack(&state),"[1][\"2.0\"][-8][3]");
        print_stack(&state);

        state.concat(3);
        assert_eq!(format_stack(&state),"[1][\"2.0-83\"]");
        print_stack(&state);

        state.push_boolean(state.compare(1, 2, CompareOp::EQ));
        assert_eq!(format_stack(&state),"[1][\"2.0-83\"][false]");
        print_stack(&state);
    }
}