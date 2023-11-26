use core::ops::{Add, Mul, Sub};
use std::fmt::Debug;

mod tags {
    pub const NIL: u8 = 0;
    pub const BOOLEAN: u8 = 1;
    pub const INTEGER: u8 = 2;
    pub const FLOAT: u8 = 3;
    pub const STRING_CONST: u8 = 4;
}

#[repr(C)]
#[derive(Clone)]
pub struct TValue {
    pub tag: u8,
    pub value: TValueInner,
}

impl Debug for TValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = "TValue::";
        match self.tag {
            tags::NIL => write!(f, "{prefix}::Nil"),
            tags::BOOLEAN => f
                .debug_tuple(&format!("{prefix}Bool"))
                .field(unsafe { &self.value.b })
                .finish(),
            tags::INTEGER => f
                .debug_tuple(&format!("{prefix}Int"))
                .field(unsafe { &self.value.i })
                .finish(),
            tags::FLOAT => f
                .debug_tuple(&format!("{prefix}Float"))
                .field(unsafe { &self.value.f })
                .finish(),
            tags::STRING_CONST => f
                .debug_tuple(&format!("{prefix}StringConst"))
                .field(unsafe { &self.value.s })
                .finish(),
            _ => f
                .debug_tuple(&format!("{prefix}Unknown"))
                .field(&self.tag)
                .finish(),
        }
    }
}

impl PartialEq for TValue {
    fn eq(&self, other: &Self) -> bool {
        match (self.tag, other.tag) {
            (tags::NIL, tags::NIL) => true,
            (tags::BOOLEAN, tags::BOOLEAN) => unsafe { self.value.b == other.value.b },
            (tags::INTEGER, tags::INTEGER) => unsafe { self.value.i == other.value.i },
            (tags::FLOAT, tags::INTEGER) => {
                let f = unsafe { self.value.f };
                if f.is_finite() && f.floor() == f {
                    return f as i64 == unsafe { other.value.i };
                }
                false
            }
            (tags::INTEGER, tags::FLOAT) => {
                let f = unsafe { other.value.f };
                if f.is_finite() && f.floor() == f {
                    return f as i64 == unsafe { self.value.i };
                }
                false
            }
            (tags::FLOAT, tags::FLOAT) => unsafe { self.value.f == other.value.f },
            (tags::STRING_CONST, tags::STRING_CONST) => unsafe { self.value.s == other.value.s },
            _ => false,
        }
    }
}

impl TValue {
    pub fn new_nil() -> Self {
        Self {
            tag: tags::NIL,
            value: TValueInner { b: false },
        }
    }

    pub fn new_bool(b: bool) -> Self {
        Self {
            tag: tags::BOOLEAN,
            value: TValueInner { b },
        }
    }
    pub fn new_int(i: i64) -> Self {
        Self {
            tag: tags::INTEGER,
            value: TValueInner { i },
        }
    }
    pub fn new_float(f: f64) -> Self {
        if f.is_finite() && f.floor() == f && f <= i64::MAX as _ && f >= i64::MIN as _ {
            return TValue::new_int(f as i64);
        }
        Self {
            tag: tags::FLOAT,
            value: TValueInner { f },
        }
    }

    pub fn new_str(s: &'static str) -> Self {
        let data = s.as_ptr();
        let len = s.len() as u32;
        Self {
            tag: tags::STRING_CONST,
            value: TValueInner {
                s: StringConst { len, data },
            },
        }
    }

    pub fn is_nan(&self) -> bool {
        if self.tag != tags::FLOAT {
            return false;
        }
        unsafe { self.value.f }.is_nan()
    }

    pub fn is_zero(&self) -> bool {
        match self.tag {
            tags::FLOAT => unsafe { self.value.f == 0.0 },
            tags::INTEGER => unsafe { self.value.i == 0 },
            _ => false,
        }
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub union TValueInner {
    pub b: bool,
    pub i: i64,
    pub f: f64,
    pub s: StringConst,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct StringConst {
    pub len: u32,
    pub data: *const u8,
}

impl Debug for StringConst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.len == 0 {
            return write!(f, r#""""#);
        }
        let data = unsafe {
            let Some(l_data) = std::ptr::slice_from_raw_parts(self.data, self.len as _).as_ref()
            else {
                return write!(f, "<invalid string>");
            };
            l_data
        };
        f.write_fmt(format_args!("{:?}", String::from_utf8_lossy(data)))
    }
}

impl PartialEq for StringConst {
    fn eq(&self, other: &Self) -> bool {
        if self.len != other.len {
            return false;
        }
        unsafe {
            let Some(l_data) = std::ptr::slice_from_raw_parts(self.data, self.len as _).as_ref()
            else {
                return false;
            };
            let Some(r_data) = std::ptr::slice_from_raw_parts(other.data, other.len as _).as_ref()
            else {
                return false;
            };
            l_data == r_data
        }
    }
}

#[export_name = "std::tvalue::init"]
pub unsafe extern "C" fn init_tvalue(ptr: *mut TValue) {
    ptr.as_mut().map(|v| *v = TValue::new_nil());
}

#[export_name = "std::tvalue::init_bool"]
pub unsafe extern "C" fn init_tvalue_bool(ptr: *mut TValue, b: bool) {
    ptr.as_mut().map(|v| *v = TValue::new_bool(b));
}

#[export_name = "std::tvalue::init_int"]
pub unsafe extern "C" fn init_tvalue_int(ptr: *mut TValue, i: i64) {
    ptr.as_mut().map(|v| *v = TValue::new_int(i));
}

#[export_name = "std::tvalue::init_float"]
pub unsafe extern "C" fn init_tvalue_float(ptr: *mut TValue, f: f64) {
    ptr.as_mut().map(|v| {
        *v = TValue::new_float(f);
    });
}

#[export_name = "std::tvalue::string_constant"]
pub unsafe extern "C" fn init_tvalue_str(ptr: *mut TValue, len: u32, data: *const u8) {
    ptr.as_mut().map(|v| {
        v.tag = tags::STRING_CONST;
        v.value = TValueInner {
            s: StringConst { len, data },
        }
    });
}

#[export_name = "std::tvalue::create_bool"]
pub unsafe extern "C" fn create_tvalue_bool(b: bool) -> TValue {
    TValue::new_bool(b)
}

#[export_name = "std::tvalue::create_integer"]
pub unsafe extern "C" fn create_tvalue_integer(i: i64) -> TValue {
    TValue::new_int(i)
}

#[export_name = "std::tvalue::create_float"]
pub unsafe extern "C" fn create_tvalue_float(v: f64) -> TValue {
    TValue::new_float(v)
}

#[export_name = "std::tvalue::create_string_const"]
pub unsafe extern "C" fn create_tvalue_string_const(v: *const u8, len: u32) -> TValue {
    TValue {
        tag: tags::STRING_CONST,
        value: TValueInner {
            s: StringConst { len, data: v },
        },
    }
}

#[export_name = "std::tvalue::is_truthy"]
pub unsafe extern "C" fn tvalue_is_truthy(ptr: *mut TValue) -> bool {
    let Some(v) = ptr.as_ref() else {
        return false;
    };
    if v.tag == 0 {
        return false;
    }
    if v.tag == tags::BOOLEAN {
        v.value.b
    } else {
        true
    }
}

#[export_name = "std::tvalue::get_tag"]
pub unsafe extern "C" fn tvalue_get_tag(ptr: *mut TValue) -> u8 {
    let Some(v) = ptr.as_ref() else {
        return 0;
    };
    v.tag
}

macro_rules! get_or_return {
    ($ptr:ident, $out:ident) => {
        if let Some(v) = $ptr.as_ref() {
            v
        } else {
            *$out = TValue::new_bool(false);
            return;
        }
    };
}

macro_rules! get_mut_or_return {
    ($ptr:ident) => {
        if let Some(v) = $ptr.as_mut() {
            v
        } else {
            return;
        }
    };
}

macro_rules! bin_math_prefix {
    ($l:ident, $r:ident, $o:ident) => {
        (
            get_or_return!($l, $o),
            get_or_return!($r, $o),
            get_mut_or_return!($o),
        )
    };
}

macro_rules! bin_math_op {
    ($l:ident, $r:ident, $o:ident, $iop:path, $fop:path) => {{
        let (l, r, o) = bin_math_prefix!($l, $r, $o);

        *o = match (l.tag, r.tag) {
            (tags::INTEGER, tags::INTEGER) => TValue::new_int($iop(l.value.i, r.value.i)),
            (tags::FLOAT, tags::FLOAT) => {
                TValue::new_float($fop(l.value.f as f64, r.value.f as f64))
            }
            (tags::INTEGER, tags::FLOAT) => TValue::new_float($fop(l.value.i as f64, r.value.f)),
            (tags::FLOAT, tags::INTEGER) => TValue::new_float($fop(l.value.f, r.value.i as f64)),
            _ => TValue::new_bool(false),
        }
    }};
}

#[export_name = "std::tvalue::add"]
pub unsafe extern "C" fn tvalue_add(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    bin_math_op!(lhs, rhs, out, i64::wrapping_add, f64::add)
}

#[export_name = "std::tvalue::sub"]
pub unsafe extern "C" fn tvalue_sub(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    bin_math_op!(lhs, rhs, out, i64::wrapping_sub, f64::sub)
}

#[export_name = "std::tvalue::mul"]
pub unsafe extern "C" fn tvalue_mul(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    bin_math_op!(lhs, rhs, out, i64::wrapping_mul, f64::mul)
}

fn tvalue_div_(l: &TValue, r: &TValue, o: &mut TValue) {
    let (l, r) = unsafe {
        match (l.tag, r.tag) {
            (tags::INTEGER, tags::INTEGER) => (l.value.i as f64, r.value.i as f64),
            (tags::INTEGER, tags::FLOAT) => (l.value.i as f64, r.value.f),
            (tags::FLOAT, tags::INTEGER) => (l.value.f, r.value.i as f64),
            (tags::FLOAT, tags::FLOAT) => (l.value.f, r.value.f),
            _ => {
                *o = TValue::new_bool(false);
                return;
            }
        }
    };
    let f = if r == 0.0 || r == -0.0 {
        if l == 0.0 || l == -0.0 {
            -f64::NAN
        } else if l.is_sign_negative() {
            -f64::INFINITY
        } else {
            f64::INFINITY
        }
    } else {
        l / r
    };
    *o = TValue::new_float(f);
}

#[export_name = "std::tvalue::div"]
pub unsafe extern "C" fn tvalue_div(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, r, o) = bin_math_prefix!(lhs, rhs, out);
    tvalue_div_(l, r, o);
}

#[export_name = "std::tvalue::floor_div"]
pub unsafe extern "C" fn tvalue_floor_div(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, r, o) = bin_math_prefix!(lhs, rhs, out);
    if r.is_zero() && l.tag == tags::INTEGER {
        *o = TValue::new_bool(false);
        return;
    }
    tvalue_div_(l, r, o);
    let f = match o.tag {
        tags::INTEGER => o.value.i as f64,
        tags::FLOAT => o.value.f,
        _ => return,
    };
    *o = TValue::new_float(f.floor());
}

pub unsafe extern "C" fn tvalue_exponent(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, r, o) = bin_math_prefix!(lhs, rhs, out);
    *o = match (l.tag, r.tag) {
        (tags::INTEGER, tags::INTEGER) => {
            let v = (l.value.i as f64).powf(r.value.i as _);
            TValue::new_float(v)
        }
        (tags::FLOAT, tags::FLOAT) => {
            let v = l.value.f.powf(r.value.f);
            TValue::new_float(v)
        }
        (tags::FLOAT, tags::INTEGER) => {
            let v = l.value.f.powf(r.value.i as f64);
            TValue::new_float(v)
        }
        (tags::INTEGER, tags::FLOAT) => {
            let v = (l.value.i as f64).powf(r.value.f);
            TValue::new_float(v)
        }
        _ => TValue::new_bool(false),
    };
}

#[export_name = "std::tvalue::negate"]
pub unsafe extern "C" fn tvalue_negate(lhs: *mut TValue, out: *mut TValue) {
    let (l, o) = (get_or_return!(lhs, out), get_mut_or_return!(out));
    *o = match l.tag {
        tags::INTEGER => TValue::new_int(-l.value.i),
        tags::FLOAT => TValue::new_float(-l.value.f),
        _ => TValue::new_bool(false),
    };
}

#[export_name = "std::tvalue::bin_and"]
pub unsafe extern "C" fn tvalue_bin_and(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, r, o) = bin_math_prefix!(lhs, rhs, out);
    let i = match (l.tag, r.tag) {
        (tags::INTEGER, tags::INTEGER) => l.value.i & r.value.i,
        _ => {
            *o = TValue::new_bool(false);
            return;
        }
    };
    *o = TValue {
        tag: tags::INTEGER,
        value: TValueInner { i },
    };
}

#[export_name = "std::tvalue::bin_or"]
pub unsafe extern "C" fn tvalue_bin_or(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, r, o) = bin_math_prefix!(lhs, rhs, out);
    let i = match (l.tag, r.tag) {
        (tags::INTEGER, tags::INTEGER) => l.value.i | r.value.i,
        _ => {
            *o = TValue::new_bool(false);
            return;
        }
    };
    *o = TValue {
        tag: tags::INTEGER,
        value: TValueInner { i },
    };
}

#[export_name = "std::tvalue::bin_xor"]
pub unsafe extern "C" fn tvalue_bin_xor(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, r, o) = bin_math_prefix!(lhs, rhs, out);
    let i = match (l.tag, r.tag) {
        (tags::INTEGER, tags::INTEGER) => l.value.i ^ r.value.i,
        _ => {
            *o = TValue::new_bool(false);
            return;
        }
    };
    *o = TValue {
        tag: tags::INTEGER,
        value: TValueInner { i },
    };
}

#[export_name = "std::tvalue::bin_rhs"]
pub unsafe extern "C" fn tvalue_bin_rhs(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, r, o) = bin_math_prefix!(lhs, rhs, out);
    let i = match (l.tag, r.tag) {
        (tags::INTEGER, tags::INTEGER) => l.value.i >> r.value.i,
        _ => {
            *o = TValue::new_bool(false);
            return;
        }
    };
    *o = TValue {
        tag: tags::INTEGER,
        value: TValueInner { i },
    };
}

#[export_name = "std::tvalue::bin_lhs"]
pub unsafe extern "C" fn tvalue_bin_lhs(lhs: *mut TValue, rhs: *mut TValue, out: *mut TValue) {
    let (l, r, o) = bin_math_prefix!(lhs, rhs, out);
    let i = match (l.tag, r.tag) {
        (tags::INTEGER, tags::INTEGER) => l.value.i << r.value.i,
        _ => {
            *o = TValue::new_bool(false);
            return;
        }
    };
    *o = TValue {
        tag: tags::INTEGER,
        value: TValueInner { i },
    };
}

#[export_name = "std::tvalue::bin_not"]
pub unsafe extern "C" fn tvalue_bin_not(lhs: *mut TValue, out: *mut TValue) {
    let (l, o) = (get_or_return!(lhs, out), get_mut_or_return!(out));
    let i = match l.tag {
        tags::INTEGER => !l.value.i,
        _ => {
            *o = TValue::new_bool(false);
            return;
        }
    };
    *o = TValue {
        tag: tags::INTEGER,
        value: TValueInner { i },
    };
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn tvalue_debug() {
        insta::assert_debug_snapshot!(&[
            TValue::new_nil(),
            TValue::new_bool(false),
            TValue::new_bool(true),
            TValue::new_int(42),
            TValue::new_float(42.1),
            TValue::new_str("hello world!"),
        ])
    }

    #[test]
    fn int_add() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);

            let expected = lua.load(&format!("{}+{}", l, r)).eval().map(|v: i64| {
                TValue::new_int(v)
            }).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                tvalue_add(&mut lhs, &mut rhs, &mut result);
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn float_add() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: f64, r: f64)| {
            let mut lhs = TValue::new_float(l);
            let mut rhs = TValue::new_float(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("{}+{}", l, r)).eval().map(|v: f64| {
                TValue::new_float(v)
            }).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                tvalue_add(&mut lhs, &mut rhs, &mut result);
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn int_neg() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("-({})", l)).eval().map(|v: i64| {
                TValue::new_int(v)
            }).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                tvalue_negate(&mut lhs, &mut result);
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn float_neg() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: f64)| {
            let mut lhs = TValue::new_float(l);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("-({})", l)).eval().map(|v: f64| {
                TValue::new_float(v)
            }).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                tvalue_negate(&mut lhs, &mut result);
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn int_sub() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("({})-({})", l, r)).eval().map(|v: i64| {
                TValue::new_int(v)
            }).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                tvalue_sub(&mut lhs, &mut rhs, &mut result);
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn float_sub() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: f64, r: f64)| {
            let mut lhs = TValue::new_float(l);
            let mut rhs = TValue::new_float(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("({})-({})", l, r)).eval().map(|v: f64| {
                TValue::new_float(v)
            }).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                tvalue_sub(&mut lhs, &mut rhs, &mut result);
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn int_mul() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("{}*{}", l, r)).eval().map(|v: i64| {
                TValue::new_int(v)
            }).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                tvalue_mul(&mut lhs, &mut rhs, &mut result);
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn float_mul() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: f64, r: f64)| {
            let mut lhs = TValue::new_float(l);
            let mut rhs = TValue::new_float(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("{}*{}", l, r)).eval().map(|v: f64| {
                TValue::new_float(v)
            }).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                tvalue_mul(&mut lhs, &mut rhs, &mut result);
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn int_div() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("{}/{}", l, r)).eval().map(|v: f64| {
                TValue::new_float(v)
            }).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                tvalue_div(&mut lhs, &mut rhs, &mut result);
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn float_div() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: f64, r: f64)| {
            let mut lhs = TValue::new_float(l);
            let mut rhs = TValue::new_float(r);
            let mut result = TValue::new_bool(false);
            unsafe {
                tvalue_div(&mut lhs, &mut rhs, &mut result);
            }
            let expected = lua.load(format!("{}/{}", l, r, )).eval().map(|v: f64| {
                TValue::new_float(v)
            }).unwrap_or_else(|_| TValue::new_bool(false));
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{:?} / {:?} != {:?} found {:?}",
                    lhs, rhs, expected, result,
                );
            }
        });
    }

    #[test]
    fn int_floor_div() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("{}//{}", l, r)).eval().map(|v: i64| {
                TValue::new_int(v)
            }).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                tvalue_floor_div(&mut lhs, &mut rhs, &mut result);
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn float_floor_div() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: f64, r: f64)| {
            let mut lhs = TValue::new_float(l);
            let mut rhs = TValue::new_float(r);
            let mut result = TValue::new_bool(false);

            unsafe {
                tvalue_floor_div(&mut lhs, &mut rhs, &mut result);
            }

            let expected = lua.load(format!("{}//{}", l, r, )).eval().map(|v: f64| {
                TValue::new_float(v)
            }).unwrap_or_else(|_| TValue::new_bool(false));
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{:?} // {:?} != {:?} found {:?}",
                    lhs, rhs, expected, result,
                );
            }
        });
    }

    #[test]
    fn int_exp() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("({})^({})", l, r)).eval().map(|v: f64| {
                TValue::new_float(v)
            }).unwrap_or_else(|_| TValue::new_bool(false));

            unsafe {
                tvalue_exponent(&mut lhs, &mut rhs, &mut result);
            }
            proptest::prop_assert_eq!(&expected, &result, "{:?} ^ {:?} != {:?} found {:?}", lhs, rhs, expected, result);
        });
    }

    #[test]
    fn float_floor_exp() {
        let lua = mlua::Lua::new();

        proptest::proptest!(|(l: f64, r: f64)| {
            let mut lhs = TValue::new_float(l);
            let mut rhs = TValue::new_float(r);
            let mut result = TValue::new_bool(false);

            unsafe {
                tvalue_exponent(&mut lhs, &mut rhs, &mut result);
            }

            let expected = lua.load(format!("({})^({})", l, r, )).eval().map(|v: f64| {
                TValue::new_float(v)
            }).unwrap_or_else(|_| TValue::new_bool(false));
            if !(expected.is_nan() && result.is_nan()) {
                proptest::prop_assert_eq!(&expected, &result, "{:?} ^ {:?} != {:?} found {:?}",
                    lhs, rhs, expected, result,
                );
            }
        });
    }

    #[test]
    fn int_bin_and() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("{l}&{r}")).eval().map(|v: i64| {
                TValue::new_int(v)
            }).unwrap_or_else(|_| TValue::new_bool(false));
            unsafe {
                tvalue_bin_and(&mut lhs, &mut rhs, &mut result);
            }
            proptest::prop_assert_eq!(&expected, &result, "{:?} & {:?} != {:?} found {:?}",
                lhs, rhs, expected, result,
            );
        });
    }

    #[test]
    fn int_bin_or() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("{l}|{r}")).eval().map(|v: i64| {
                TValue::new_int(v)
            }).unwrap_or_else(|_| TValue::new_bool(false));
            unsafe {
                tvalue_bin_or(&mut lhs, &mut rhs, &mut result);
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn int_bin_xor() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64, r: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut rhs = TValue::new_int(r);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("{l}~{r}")).eval().map(|v: i64| {
                TValue::new_int(v)
            }).unwrap_or_else(|_| TValue::new_bool(false));
            unsafe {
                tvalue_bin_xor(&mut lhs, &mut rhs, &mut result);
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }

    #[test]
    fn int_bin_not() {
        let lua = mlua::Lua::new();
        proptest::proptest!(|(l: i64)| {
            let mut lhs = TValue::new_int(l);
            let mut result = TValue::new_bool(false);
            let expected = lua.load(&format!("~{l}")).eval().map(|v: i64| {
                TValue::new_int(v)
            }).unwrap_or_else(|_| TValue::new_bool(false));
            unsafe {
                tvalue_bin_not(&mut lhs, &mut result);
            }
            proptest::prop_assert_eq!(expected, result);
        });
    }
}
