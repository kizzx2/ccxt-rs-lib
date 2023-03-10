#![allow(clippy::all)]
#![allow(dead_code)]
#![allow(unreachable_code)]
#![allow(unused_assignments)]
#![allow(unused_comparisons)]
#![allow(unused_mut)]
#![allow(unused_variables)]

use async_trait::async_trait;
use rand::RngCore;
use std::cmp::{max, Ordering};
use chrono::{DateTime, NaiveDateTime, Utc};
use regex::Regex;
use serde::{Serialize, Deserialize};
use std::ops::{Add, Div, Mul, Not, Rem, Sub};
use std::str::FromStr;
use std::time::{SystemTime, UNIX_EPOCH};
use hmac::{Mac};
use sha2::{Digest};
use num_bigint::BigInt;
use num_traits::{ToPrimitive, Zero};
use num_traits::sign::Signed;
use num_integer::Integer;
use serde_json::json;

pub const PRECISE_BASE: usize = 10;

// rounding mode
pub const TRUNCATE: usize = 0;
pub const ROUND: usize = 1;
pub const ROUND_UP: usize = 2;
pub const ROUND_DOWN: usize = 3;

// digits counting mode
pub const DECIMAL_PLACES: usize = 2;
pub const SIGNIFICANT_DIGITS: usize = 3;
pub const TICK_SIZE: usize = 4;

// padding mode
pub const NO_PADDING: usize = 5;
pub const PAD_WITH_ZERO: usize = 6;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Value {
    Json(serde_json::Value),
    Precise(Precise),
    Undefined,
}

pub fn normalize(x: &Value) -> Option<serde_json::Value> {
    match x {
        Value::Json(v) => {
            match v {
                serde_json::Value::String(s) => match s.as_str() {
                    "Undefined" => None,
                    _ => Some(v.clone())
                },
                serde_json::Value::Number(n) => Some(v.clone()),
                serde_json::Value::Bool(b) => Some(v.clone()),
                serde_json::Value::Null => Some(v.clone()),
                serde_json::Value::Array(a) => Some(serde_json::Value::Array(a.into_iter().map(|x| {
                    if x.is_object() { normalize(&Value::Json(x.clone())).unwrap() } else { x.clone() }
                }).collect())),
                serde_json::Value::Object(o) => if o.contains_key("Json") {
                    normalize(&Value::Json(o.get("Json").unwrap().clone()))
                } else {
                    let mut m = serde_json::Map::new();
                    for k in o.keys() {
                        let v = normalize(&Value::Json(o.get(k).unwrap().clone()));
                        if v.is_some() {
                            let v1 = v.unwrap();
                            m.insert(k.to_string(), v1);
                        }
                    }
                    Some(serde_json::Value::Object(m))
                }
            }
        },
        Value::Precise(v) => unimplemented!(),
        Value::Undefined => None,
    }
}

pub trait ValueTrait {
    fn is_undefined(&self) -> bool;
    fn is_nullish(&self) -> bool;
    fn is_nonnullish(&self) -> bool;
    fn is_truthy(&self) -> bool;
    fn or_default(&self, default: Value) -> Value;
    fn is_number(&self) -> bool;
    fn is_string(&self) -> bool;
    fn is_object(&self) -> bool;
    fn is_falsy(&self) -> bool;
    fn to_upper_case(&self) -> Value;
    fn unwrap_str(&self) -> &str;
    fn unwrap_usize(&self) -> usize;
    fn unwrap_bool(&self) -> bool;
    fn unwrap_precise(&self) -> &Precise;
    fn unwrap_json(&self) -> &serde_json::Value;
    fn unwrap_json_mut(&mut self) -> &mut serde_json::Value;
    fn unwrap_precise_mut(&mut self) -> &mut Precise;
    fn len(&self) -> usize;
    fn get(&self, key: Value) -> Value;
    fn set(&mut self, key: Value, value: Value);
    fn push(&mut self, value: Value);
    fn split(&self, separator: Value) -> Value;
    fn contains_key(&self, key: Value) -> bool;
    fn join(&self, glue: Value) -> Value;
    fn keys(&self) -> Vec<Value>;
    fn values(&self) -> Vec<Value>;
    fn to_array(&self, x: Value) -> Value;
    fn index_of(&self, x: Value) -> Value;
    fn typeof_(&self) -> Value;
    fn to_string(&self) -> Value;
    fn slice(&self, start: Value) -> Value;
}

pub fn shift_2(x: Value) -> (Value, Value) {
    match x.unwrap_json() {
        serde_json::Value::Array(x) => {
            if x.len() >= 2 {
                (x[0].clone().into(), x[1].clone().into())
            } else {
                panic!("array length is not >= 2")
            }
        }
        _ => panic!("type error"),
    }
}

impl Value {
    pub fn new_array() -> Self {
        Value::Json(serde_json::Value::Array(vec![]))
    }

    fn null() -> Self {
        Value::Json(serde_json::Value::Null)
    }

    pub fn new_object() -> Self {
        Value::Json(serde_json::Value::Object(serde_json::Map::new()))
    }
}

impl ValueTrait for Value {
    fn is_undefined(&self) -> bool {
        match self {
            Value::Undefined => true,
            _ => false
        }
    }

    fn is_nullish(&self) -> bool {
        match self {
            Value::Undefined => true,
            Value::Json(x) => x.is_null(),
            _ => false
        }
    }

    fn is_nonnullish(&self) -> bool {
        match self {
            Value::Undefined => false,
            Value::Json(x) => !x.is_null(),
            _ => true
        }
    }

    fn is_truthy(&self) -> bool {
        match self {
            Value::Json(json) => {
                match json {
                    serde_json::Value::Bool(b) => *b,
                    serde_json::Value::Null => false,
                    serde_json::Value::Number(n) => {
                        if n.is_f64() {
                            n.as_f64().unwrap() != 0.0
                        } else if n.is_u64() {
                            n.as_i64().unwrap() != 0
                        } else if n.is_i64() {
                            n.as_i64().unwrap() != 0
                        } else {
                            unreachable!()
                        }
                    }
                    serde_json::Value::String(s) => !s.is_empty(),
                    serde_json::Value::Array(a) => !a.is_empty(),
                    serde_json::Value::Object(o) => !o.is_empty(),
                    // _ => false,
                }
            }
            Value::Precise(precise) => !precise.is_zero(),
            _ => false,
        }
    }

    fn or_default(&self, default: Value) -> Value {
        match self {
            Value::Undefined => default,
            _ => self.clone()
        }
    }

    fn is_number(&self) -> bool {
        match self {
            Value::Json(j) => j.is_number(),
            _ => false
        }
    }

    fn is_string(&self) -> bool {
        match self {
            Value::Json(j) => j.is_string(),
            _ => false
        }
    }

    fn is_object(&self) -> bool {
        match self {
            Value::Json(j) => j.is_object(),
            _ => false
        }
    }

    fn is_falsy(&self) -> bool {
        !self.is_truthy()
        // match self {
        //     Value::Json(v) => v.is_null(),
        //     Value::Undefined => true,
        //     _ => false
        // }
    }

    fn to_upper_case(&self) -> Value {
        match self {
            Value::Json(v) => Value::Json(v.to_string().to_uppercase().parse().unwrap()),
            _ => panic!("unexpected value")
        }
    }

    fn unwrap_str(&self) -> &str {
        match self {
            Value::Json(v) => v.as_str().unwrap(),
            _ => panic!("unexpected value")
        }
    }

    fn unwrap_usize(&self) -> usize {
        match self {
            Value::Json(v) => v.as_u64().unwrap() as usize,
            _ => panic!("unexpected value")
        }
    }

    fn unwrap_bool(&self) -> bool {
        match self {
            Value::Json(v) => v.as_bool().unwrap(),
            _ => panic!("unexpected value")
        }
    }

    fn unwrap_precise(&self) -> &Precise {
        match self {
            Value::Precise(v) => v,
            _ => panic!("unexpected value")
        }
    }

    fn unwrap_json(&self) -> &serde_json::Value {
        match self {
            Value::Json(v) => v,
            _ => panic!("unexpected value")
        }
    }

    fn unwrap_json_mut(&mut self) -> &mut serde_json::Value {
        match self {
            Value::Json(v) => v,
            _ => panic!("unexpected value")
        }
    }

    fn unwrap_precise_mut(&mut self) -> &mut Precise {
        match self {
            Value::Precise(v) => v,
            _ => panic!("unexpected value")
        }
    }

    fn len(&self) -> usize {
        match self {
            Value::Json(v) => if v.is_array() { v.as_array().unwrap().len() } else { 0 },
            _ => panic!("unexpected value")
        }
    }

    fn get(&self, key: Value) -> Value {
        match self {
            Value::Json(v) => {
                if key.is_string() {
                    match v.get(key.unwrap_str()) {
                        Some(v) => {
                            if v.is_object() && v.as_object().unwrap().contains_key("Json") {
                                Value::Json(v.as_object().unwrap().get("Json").unwrap().clone())
                            } else {
                                Value::Json(v.clone())
                            }
                        },
                        None => Value::Undefined
                    }
                } else if key.is_number() {
                    match v.get(key.unwrap_usize()) {
                        Some(v) => {
                            if v.is_object() && v.as_object().unwrap().contains_key("Json") {
                                Value::Json(v.as_object().unwrap().get("Json").unwrap().clone())
                            } else {
                                Value::Json(v.clone())
                            }
                        },
                        None => Value::Undefined
                    }
                } else {
                    panic!("unexpected value {:?}", key)
                }
            }
            _ => panic!("unexpected value {:?}", self)
        }
    }

    fn set(&mut self, key: Value, value: Value) {
        match self {
            Value::Json(v) => {
                match value {
                    Value::Json(v1) => {
                        if v.is_object() {
                            v.as_object_mut().unwrap().insert(key.unwrap_str().to_string(), v1.clone());
                        }
                    }
                    Value::Undefined => {
                        if v.is_object() {
                            v.as_object_mut().unwrap().remove(key.unwrap_str());
                        }
                    }
                    _ => panic!("unexpected value")
                }
            }
            _ => panic!("unexpected value")
        }
    }

    fn push(&mut self, value: Value) {
        match self {
            Value::Json(v) => {
                v.as_array_mut().unwrap().push(value.unwrap_json().clone());
            }
            _ => panic!("unexpected value")
        }
    }

    fn split(&self, separator: Value) -> Value {
        Value::Json(serde_json::Value::Array(
            self.unwrap_str().split(separator.unwrap_str()).into_iter().map(Into::into).collect()))
    }

    fn contains_key(&self, key: Value) -> bool {
        match self {
            Value::Json(v) => if v.is_object() { v.as_object().unwrap().contains_key(key.unwrap_str()) } else { false },
            _ => panic!("unexpected value")
        }
    }

    fn keys(&self) -> Vec<Value> {
        match self {
            Value::Json(v) => {
                v.as_object().unwrap().keys().map(|x| Value::Json(serde_json::Value::String(x.to_string()))).collect()
            }
            _ => panic!("unexpected value")
        }
    }

    fn values(&self) -> Vec<Value> {
        match self {
            Value::Json(v) => {
                v.as_object().unwrap().values().map(|x| Value::Json(x.clone())).collect()
            }
            _ => panic!("unexpected value")
        }
    }

    fn to_array(&self, x: Value) -> Value {
        match x {
            Value::Json(v) if v.is_object() => {
                Value::Json(serde_json::Value::Array(v.as_object().unwrap().values().into_iter().map(|x| x.clone()).collect()))
            }
            _ => x
        }
    }

    fn index_of(&self, x: Value) -> Value {
        match self {
            Value::Json(v) if v.is_string() => {
                let i: i64 = match v.as_str().unwrap().find(x.unwrap_str()) {
                    Some(x) => x.try_into().unwrap_or(-1),
                    None => -1
                };
                Value::Json(serde_json::Value::Number(i.try_into().unwrap()))
            }
            _ => Value::Undefined
        }
    }

    fn join(&self, glue: Value) -> Value {
        match self {
            Value::Json(v) if v.is_array() => {
                Value::Json(serde_json::Value::String(v.as_array().unwrap().iter().map(|x| x.as_str().unwrap()).collect::<Vec<&str>>().join(glue.unwrap_str())))
            }
            _ => Value::Undefined
        }
    }

    fn to_string(&self) -> Value {
        match self {
            Value::Json(v) => Value::Json(serde_json::Value::String(v.to_string())),
            _ => panic!("unexpected value")
        }
    }

    fn typeof_(&self) -> Value {
        match self {
            Value::Json(v) => match v {
                serde_json::Value::Null => Value::Json(serde_json::Value::String("object".to_string())),
                serde_json::Value::Bool(_) => Value::Json(serde_json::Value::String("boolean".to_string())),
                serde_json::Value::Number(_) => Value::Json(serde_json::Value::String("number".to_string())),
                serde_json::Value::String(_) => Value::Json(serde_json::Value::String("string".to_string())),
                serde_json::Value::Array(_) => Value::Json(serde_json::Value::String("object".to_string())),
                serde_json::Value::Object(_) => Value::Json(serde_json::Value::String("object".to_string())),
                // _ => Value::Json(serde_json::Value::String("undefined".to_string()))
            }
            _ => Value::Undefined
        }
    }

    fn slice(&self, start: Value) -> Value {
        todo!()
    }
}

pub fn parse_int(x: Value) -> Value {
    match x {
        Value::Json(v) if v.is_number() => {
            let w: u64 = if v.is_i64() {
                v.as_i64().unwrap().try_into().unwrap()
            } else if v.is_f64() {
                v.as_f64().unwrap().round() as u64
            } else if v.is_u64() {
                v.as_u64().unwrap()
            } else {
                panic!("unexpected value")
            };
            w.into()
        }
        _ => Value::Undefined
    }
}

pub fn extend_2(x: Value, y: Value) -> Value {
    let mut x1 = x.unwrap_json().clone();
    let mut y1 = y.unwrap_json().clone();
    let x = x1.as_object_mut().unwrap();
    let y = y1.as_object_mut().unwrap();
    for (k, v) in y {
        x.insert(k.to_owned(), v.clone());
    }
    serde_json::Value::Object(x.clone()).into()
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Precise {
    value: BigInt,
    decimals: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Object {}

impl Object {
    pub fn keys(x: Value) -> Value {
        Value::Json(serde_json::Value::Array(x.keys().into_iter().map(|x| x.unwrap_json().clone()).collect()))
    }

    pub fn values(x: Value) -> Value {
        Value::Json(serde_json::Value::Array(x.values().into_iter().map(|x| x.unwrap_json().clone()).collect()))
    }
}

pub struct JSON {}
impl JSON {
    pub fn parse(x: Value) -> Value {
        Value::Json(serde_json::from_str(x.unwrap_str()).unwrap())
    }
}

pub struct Math {}

impl Math {
    pub fn max(x: Value, y: Value) -> Value {
        match (x, y) {
            (Value::Json(v1), Value::Json(v2)) if v1.is_number() && v2.is_number() => {
                Value::Json(if v1.as_f64().unwrap() > v2.as_f64().unwrap() {
                    v1.clone()
                } else {
                    v2.clone()
                })
            }
            _ => Value::Undefined
        }
    }

    pub fn min(x: Value, y: Value) -> Value {
        match (x, y) {
            (Value::Json(v1), Value::Json(v2)) if v1.is_number() && v2.is_number() => {
                Value::Json(if v1.as_f64().unwrap() < v2.as_f64().unwrap() {
                    v1.clone()
                } else {
                    v2.clone()
                })
            }
            _ => Value::Undefined
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Array {}

impl Array {
    pub fn is_array(x: Value) -> Value {
        matches!(x, Value::Json(v) if v.is_array()).into()
    }
}


impl Precise {
    pub fn new(val: Value) -> Value {
        let x = val.unwrap_str();
        let mut modifier = 0;
        let mut number = x.to_lowercase();
        if number.contains('e') {
            let splits = number.split('e').map(|x| x.to_owned()).collect::<Vec<_>>();
            number = splits.get(0).unwrap().to_string();
            modifier = splits.get(1).unwrap().parse::<i32>().unwrap();
        }
        let decimals = match number.find('.') {
            Some(i) => number.len() - i - 1,
            None => 0,
        };
        let integer_string = number.replace(".", "");
        Self::new_with_decimals(integer_string.into(), (decimals - modifier as usize).try_into().unwrap())
    }

    pub fn new_with_decimals(val: Value, decimals: u32) -> Value {
        Value::Precise(Self {
            value: BigInt::from_str(val.unwrap_str().try_into().unwrap()).unwrap(),
            decimals,
        })
    }

    pub fn mul(&self, other: &Value) -> Value {
        Value::Precise(Self {
            value: self.value.checked_mul(&other.unwrap_precise().value).unwrap(),
            decimals: self.decimals + other.unwrap_precise().decimals,
        })
    }

    pub fn div(&self, other: &Value, precision: Option<u32>) -> Value {
        let precision1 = precision.unwrap_or(18);
        let distance: i32 = (precision1 - self.decimals + other.unwrap_precise().decimals).try_into().unwrap();
        let numerator = if distance == 0 {
            self.value.clone()
        } else if distance < 0 {
            self.value.checked_div(&BigInt::from(PRECISE_BASE).pow(distance.abs().try_into().unwrap())).unwrap()
        } else {
            self.value.checked_mul(&BigInt::from(PRECISE_BASE).pow(distance.abs().try_into().unwrap())).unwrap()
        };
        Value::Precise(Self {
            value: numerator.div(&other.unwrap_precise().value),
            decimals: precision1,
        })
    }

    pub fn add(&self, other: &Value) -> Value {
        let other = other.unwrap_precise();
        Value::Precise(if self.decimals == other.decimals {
            Self {
                value: self.value.checked_add(&other.value).unwrap(),
                decimals: self.decimals,
            }
        } else {
            let (smaller, bigger) = if self.decimals > other.decimals {
                (other, self)
            } else {
                (self, other)
            };
            let exponent = bigger.decimals - smaller.decimals;
            let normalised = smaller.value.checked_mul(&BigInt::from(PRECISE_BASE).pow(exponent)).unwrap();
            let result = normalised.add(&bigger.value);
            Self {
                value: result,
                decimals: bigger.decimals,
            }
        })
    }

    pub fn r#mod(&self, other: &Value) -> Value {
        let other = other.unwrap_precise();
        // XXX
        let rationizer_numerator: u32 = max(-(self.decimals as i32) + other.decimals as i32, 0).try_into().unwrap();
        let numerator = self.value.checked_mul(&BigInt::from(PRECISE_BASE).pow(rationizer_numerator)).unwrap();
        let rationizer_denominator: u32 = max(-(other.decimals as i32) + self.decimals as i32, 0).try_into().unwrap();
        let denominator = other.value.checked_mul(&BigInt::from(PRECISE_BASE).pow(rationizer_denominator)).unwrap();
        let result = numerator.mod_floor(&denominator);
        Value::Precise(Self {
            value: result,
            decimals: rationizer_denominator + other.decimals,
        })
    }

    pub fn sub(&self, other: &Value) -> Value {
        let other = other.unwrap_precise();
        self.add(&other.neg())
    }

    pub fn abs(&self) -> Value {
        Value::Precise(Self {
            value: self.value.abs(),
            decimals: self.decimals,
        })
    }

    pub fn neg(&self) -> Value {
        Value::Precise(Self {
            value: self.value.checked_mul(&BigInt::from(-1)).unwrap(),
            decimals: self.decimals,
        })
    }

    pub fn min(&self, other: &Value) -> Value {
        todo!()
    }

    pub fn max(&self, other: &Value) -> &Value {
        todo!()
        // if self.gt(other) { self } else { other }
    }

    pub fn gt(&self, other: &Value) -> bool {
        self.sub(other).unwrap_precise().value.is_positive()
    }

    pub fn ge(&self, other: &Value) -> bool {
        self.gt(other) || self.eq(other.unwrap_precise())
    }

    pub fn lt(&self, other: &Value) -> bool {
        self.sub(other).unwrap_precise().value.is_negative()
    }

    pub fn le(&self, other: &Value) -> bool {
        self.lt(other) || self.eq(other.unwrap_precise())
    }

    pub fn reduce(&mut self) {
        let string = self.value.to_string();
        let start = string.len() - 1;
        if start == 0 {
            if start == 0 {
                self.decimals = 0;
            }
            return;
        }
        let mut i = 0;
        let chars = string.chars().collect::<Vec<_>>();
        for i in (0..=start).rev() {
            if chars[i] != '0' {
                break;
            }
        }
        let difference = start - i;
        if difference == 0 {
            return;
        }

        self.decimals -= difference as u32;
        self.value = BigInt::from_str(&string[0..=i]).unwrap()
    }

    pub fn is_zero(&self) -> bool {
        self.value.is_zero()
    }

    pub fn equals(&mut self, other: &mut Value) -> bool {
        let other = other.unwrap_precise_mut();
        self.reduce();
        other.reduce();
        self.value == other.value && self.decimals == other.decimals
    }

    pub fn string_mul(x: Value, y: Value) -> Value {
        Precise::new(x).unwrap_precise().mul(&Precise::new(y))
    }

    pub fn string_div(x: Value, y: Value, precision: Value) -> Value {
        Precise::new(x).unwrap_precise().div(&Precise::new(y), if precision.is_undefined() {
            None
        } else {
            Some(precision.unwrap_precise().value.clone().try_into().unwrap())
        })
    }

    pub fn string_add(x: Value, y: Value) -> Value {
        Precise::new(x).unwrap_precise().add(&Precise::new(y))
    }

    pub fn string_sub(x: Value, y: Value) -> Value {
        Precise::new(x).unwrap_precise().sub(&Precise::new(y))
    }

    pub fn string_abs(x: Value) -> Value {
        Precise::new(x).unwrap_precise().abs()
    }

    pub fn string_neg(x: Value) -> Value {
        Precise::new(x).unwrap_precise().neg()
    }

    pub fn string_mod(x: Value, y: Value) -> Value {
        Precise::new(x).unwrap_precise().r#mod(&Precise::new(y))
    }

    pub fn string_equals(x: Value, y: Value) -> bool {
        Precise::new(x).unwrap_precise().eq(&Precise::new(y).unwrap_precise())
    }

    pub fn string_eq(x: Value, y: Value) -> bool {
        Precise::new(x).unwrap_precise().eq(&Precise::new(y).unwrap_precise())
    }

    pub fn string_min(x: Value, y: Value) -> Value {
        let x1 = Precise::new(x);
        let y1 = Precise::new(y);
        if x1.lt(&y1) { x1 } else { y1 }
    }

    pub fn string_max(x: Value, y: Value) -> Value {
        let x1 = Precise::new(x);
        let y1 = Precise::new(y);
        if x1.gt(&y1) { x1 } else { y1 }
    }

    pub fn string_gt(x: Value, y: Value) -> bool {
        Precise::new(x).gt(&Precise::new(y))
    }

    pub fn string_ge(x: Value, y: Value) -> bool {
        Precise::new(x).ge(&Precise::new(y))
    }

    pub fn string_lt(x: Value, y: Value) -> bool {
        Precise::new(x).lt(&Precise::new(y))
    }

    pub fn string_le(x: Value, y: Value) -> bool {
        Precise::new(x).le(&Precise::new(y))
    }
}

impl ToString for Precise {
    fn to_string(&self) -> String {
        // self.reduce(); // XXX
        let (sign, abs) = if self.value.is_negative() {
            ("-", self.value.abs())
        } else {
            ("", self.value.clone())
        };
        let abs_string = abs.to_string();
        if abs_string == "0" {
            return "0".to_string();
        }
         if abs_string.len() < self.decimals as usize {
            format!("0.{}", "0".repeat(self.decimals as usize - abs_string.len())).to_string()
            // let mut array = vec!["0"; (self.decimals - abs_string.len() as u32) as usize];
            // todo!()
            // array.extend(abs_string.chars().collect());
            // array
        } else {
            todo!()
            // abs_string.chars().collect()
        }
        // let index = integer_array.len() as u32 - self.decimals;
        // let item = if index == 0 {
        //     "0."
        // } else if self.decimals < 0 {
        //     todo!()
        //     // "0".repeat(self.decimals as usize)
        // } else if self.decimals == 0 {
        //     ""
        // } else {
        //     "."
        // };
        // todo!()
        // format!("{}{}{}{}", sign, &integer_array[0..index].iter().collect(), item, &integer_array[index..].iter().collect())
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Value::Json(serde_json::Value::String(s.to_string()))
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::Json(serde_json::Value::String(s))
    }
}

impl From<&String> for Value {
    fn from(s: &String) -> Self {
        Value::Json(serde_json::Value::String(s.to_owned()))
    }
}

impl From<i32> for Value {
    fn from(i: i32) -> Self {
        Value::Json(serde_json::Value::Number(serde_json::Number::from(i)))
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Value::Json(serde_json::Value::Number(serde_json::Number::from(i)))
    }
}

impl From<u64> for Value {
    fn from(i: u64) -> Self {
        Value::Json(serde_json::Value::Number(serde_json::Number::from(i)))
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Value::Json(serde_json::Value::Number(serde_json::Number::from_f64(f).unwrap()))
    }
}

impl From<usize> for Value {
    fn from(i: usize) -> Self {
        Value::Json(serde_json::Value::Number(serde_json::Number::from(i)))
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Json(serde_json::Value::Bool(b))
    }
}

impl<T: Into<serde_json::Value>> From<Vec<T>> for Value {
    fn from(v: Vec<T>) -> Self {
        Value::Json(serde_json::Value::Array(v.into_iter().map(|x| x.into()).collect()))
    }
}

impl From<serde_json::Value> for Value {
    fn from(v: serde_json::Value) -> Self {
        Value::Json(v)
    }
}

impl From<&serde_json::Value> for Value {
    fn from(v: &serde_json::Value) -> Self {
        Value::Json(v.clone())
    }
}

impl Into<serde_json::Value> for Value {
    fn into(self) -> serde_json::Value {
        match self {
            Value::Json(v) => v.clone(),
            Value::Undefined => json!(null),
            _ => todo!()
        }
    }
}

impl Into<usize> for Value {
    fn into(self) -> usize {
        self.unwrap_json().as_u64().unwrap() as usize
    }
}

impl Not for Value {
    type Output = Value;
    fn not(self) -> Self::Output {
        match self {
            Value::Json(v) => Value::Json(serde_json::Value::Bool(!v.as_bool().unwrap())),
            Value::Undefined => Value::Json(serde_json::Value::Bool(true)),
            _ => panic!("Not not implemented for {:?}", self),
        }
    }
}

impl Add for Value {
    type Output = Value;
    fn add(self, other: Value) -> Self::Output {
        match (self, other) {
            (Value::Json(v1), Value::Json(v2)) if v1.is_string() || v2.is_string() => {
                let s1 = if v1.is_string() { v1.as_str().unwrap().to_string() } else { v1.to_string() };
                let s2 = if v2.is_string() { v2.as_str().unwrap().to_string() } else { v2.to_string() };
                Value::Json(serde_json::Value::String(format!("{}{}", s1, s2)))
            }
            (Value::Json(v1), Value::Json(v2)) if v1.is_number() && v2.is_number() => {
                if v1.is_u64() && v2.is_u64() {
                    (v1.as_u64().unwrap() + v2.as_u64().unwrap()).into()
                } else if v1.is_i64() && v2.is_i64() {
                    (v1.as_i64().unwrap() + v2.as_i64().unwrap()).into()
                } else {
                    (v1.as_f64().unwrap() + v2.as_f64().unwrap()).into()
                }
            }
            (Value::Json(v1), Value::Undefined) => Value::Undefined,
            (Value::Undefined, Value::Json(v2)) => Value::Undefined,
            (Value::Undefined, Value::Undefined) => Value::Undefined,
            _ => Value::Undefined
        }
    }
}

impl Rem for Value {
    type Output = Value;
    fn rem(self, other: Value) -> Self::Output {
        match (self, other) {
            (Value::Json(v1), Value::Json(v2)) if v1.is_number() && v2.is_number() => {
                if v1.is_u64() && v2.is_u64() {
                    (v1.as_f64().unwrap() % v2.as_f64().unwrap()).into()
                } else if v1.is_i64() && v2.is_i64() {
                    (v1.as_f64().unwrap() % v2.as_f64().unwrap()).into()
                } else {
                    (v1.as_f64().unwrap() % v2.as_f64().unwrap()).into()
                }
            }
            (Value::Json(v1), Value::Undefined) => Value::Undefined,
            (Value::Undefined, Value::Json(v2)) => Value::Undefined,
            (Value::Undefined, Value::Undefined) => Value::Undefined,
            _ => Value::Undefined
        }
    }
}

impl Mul for Value {
    type Output = Value;
    fn mul(self, other: Value) -> Self::Output {
        match (self, other) {
            (Value::Json(x), Value::Json(y)) if x.is_number() && y.is_number() => {
                if x.is_u64() && y.is_u64() {
                    (x.as_u64().unwrap() * y.as_u64().unwrap()).into()
                } else if x.is_i64() && y.is_i64() {
                    (x.as_i64().unwrap() * y.as_i64().unwrap()).into()
                } else {
                    (x.as_f64().unwrap() * y.as_f64().unwrap()).into()
                }
            },
            _ => panic!("type error"),
        }
    }
}

impl Sub for Value {
    type Output = Value;
    fn sub(self, other: Value) -> Self::Output {
        match (self, other) {
            (Value::Json(x), Value::Json(y)) if x.is_number() && y.is_number() => {
                if x.is_u64() && y.is_u64() {
                    (x.as_u64().unwrap() - y.as_u64().unwrap()).into()
                } else if x.is_i64() && y.is_i64() {
                    (x.as_i64().unwrap() - y.as_i64().unwrap()).into()
                } else {
                    (x.as_f64().unwrap() - y.as_f64().unwrap()).into()
                }
            },
            _ => panic!("type error"),
        }
    }
}

impl Div for Value {
    type Output = Value;
    fn div(self, other: Value) -> Self::Output {
        match (self, other) {
            (Value::Json(x), Value::Json(y)) if x.is_number() && y.is_number() => {
                if x.is_u64() && y.is_u64() {
                    (x.as_u64().unwrap() / y.as_u64().unwrap()).into()
                } else if x.is_i64() && y.is_i64() {
                    (x.as_i64().unwrap() / y.as_i64().unwrap()).into()
                } else {
                    (x.as_f64().unwrap() / y.as_f64().unwrap()).into()
                }
            },
            _ => panic!("type error"),
        }
    }
}

impl Into<bool> for Value {
    fn into(self) -> bool {
        match self {
            Value::Json(v) => v.as_bool().unwrap(),
            Value::Undefined => false,
            _ => panic!("type error"),
        }
    }
}

impl PartialOrd<Self> for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let left = match &self {
            Value::Json(_) => self.unwrap_json().clone(),
            Value::Precise(x) => match x.to_string().parse::<f64>() {
                Ok(y) => serde_json::Value::from(y),
                Err(_) => serde_json::Value::Null
            }
            Value::Undefined => serde_json::Value::Null,
        };

        let right = match &other {
            Value::Json(_) => other.unwrap_json().clone(),
            Value::Precise(x) => match x.to_string().parse::<f64>() {
                Ok(y) => serde_json::Value::from(y),
                Err(_) => serde_json::Value::Null
            }
            Value::Undefined => serde_json::Value::Null,
        };

        match (&left, &right) {
            (serde_json::Value::Number(x), serde_json::Value::Number(y)) => {
                if x.is_f64() {
                    if y.is_f64() {
                        x.as_f64().unwrap().partial_cmp(&y.as_f64().unwrap())
                    } else if y.is_u64() {
                        x.as_f64().unwrap().partial_cmp(&(y.as_u64().unwrap() as u32).try_into().unwrap())
                    } else if y.is_i64() {
                        x.as_f64().unwrap().partial_cmp(&(y.as_i64().unwrap() as i32).try_into().unwrap())
                    } else {
                        panic!("unexpected type")
                    }
                } else if y.is_f64() {
                    if x.is_u64() {
                        x.as_u64().unwrap().partial_cmp(&(y.as_u64().unwrap() as u32).try_into().unwrap())
                    } else if x.is_i64() {
                        x.as_i64().unwrap().partial_cmp(&(y.as_i64().unwrap() as i32).try_into().unwrap())
                    } else {
                        panic!("unexpected type")
                    }
                } else {
                    x.as_i64().unwrap().partial_cmp(&y.as_i64().unwrap())
                }
            }
            (serde_json::Value::String(x), serde_json::Value::String(y)) => x.partial_cmp(y),
            (serde_json::Value::Bool(x), serde_json::Value::Bool(y)) => x.partial_cmp(&y),
            (serde_json::Value::Null, serde_json::Value::Null) => Some(Ordering::Equal),
            (serde_json::Value::Null, _) => Some(Ordering::Less),
            (_, serde_json::Value::Null) => Some(Ordering::Greater),
            _ => None,
        }
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

pub struct ExchangeImpl;
impl ExchangeImpl {
    pub fn init(x: &mut Value) {
        x.set("required_credentials".into(),  Value::Json(json!({
            "apiKey": true,
            "secret": true,
            "uid": false,
            "login": false,
            "password": false,
            "twofa": false,
            "privateKey": false,
            "walletAddress": false,
            "token": false,
        })));
    }
}

#[async_trait]
pub trait Exchange: ValueTrait + Sync + Send {
    fn set_number_mode(&mut self, mode: Value) {
        self.set("___number_mode".into(), mode);
    }

    fn describe(&self) -> Value {
        Value::new_object()
    }

    fn parse_number(&self, value: Value, default: Value) -> Value {
        if value.is_undefined() {
            return default;
        }

        match value.clone() {
            Value::Json(x) => {
                if x.is_number() {
                    value
                } else if x.is_string() {
                    Value::Json(serde_json::Value::Number(serde_json::Number::from_f64(x.as_str().unwrap().parse::<f64>().unwrap()).unwrap()))
                } else {
                    default
                }
            }
            _ => return default,
        }
    }

    fn extend_1(&self, x: Value) -> Value {
        x
    }

    fn extend_2(&self, x: Value, y: Value) -> Value {
        let mut x1 = x.unwrap_json().clone();
        let mut y1 = y.unwrap_json().clone();
        let x = x1.as_object_mut().unwrap();
        let y = y1.as_object_mut().unwrap();
        for (k, v) in y {
            x.insert(k.to_owned(), v.clone());
        }
        serde_json::Value::Object(x.clone()).into()
    }

    fn deep_extend_2(&self, x1: Value, x2: Value) -> Value {
        let mut result = Value::Undefined;
        for arg in [&x1, &x2] {
            if !arg.is_undefined() && arg.unwrap_json().is_object() {
                if result.is_undefined() || !result.unwrap_json().is_object() {
                    result = Value::Json(json!({}));
                }
                let result1 = result.clone();
                for key in arg.unwrap_json().as_object().unwrap().keys() {
                    // let val = self.deep_extend_2(
                    //         if result1.contains_key(key.into()) { result1.get(key.into()) } else { Value::Undefined },
                    //         arg.get(key.into()),
                    //     );
                    let val = arg.get(key.into());
                    if !val.is_undefined() {
                        result.unwrap_json_mut().as_object_mut().unwrap().insert(
                            key.to_owned(), val.unwrap_json().clone(),
                        );
                    }
                }
            }
        }
        result
    }


    fn deep_extend_3(&self, x1: Value, x2: Value, x3: Value) -> Value {
        let mut result = Value::Undefined;
        for arg in [&x1, &x2, &x3] {
            if arg.unwrap_json().is_object() {
                if result.is_undefined() || !result.unwrap_json().is_object() {
                    result = Value::Json(json!({}));
                }
                for key in arg.unwrap_json().as_object().unwrap().keys() {
                    let result1 = result.clone();
                    result.unwrap_json_mut().as_object_mut().unwrap().insert(
                        key.to_owned(), self.deep_extend_2(
                            if result1.contains_key(key.into()) { result1.get(key.into()) } else { Value::Undefined },
                            arg.get(key.into()),
                        ).unwrap_json().clone(),
                    );
                }
            }
        }
        result
    }

    fn deep_extend_4(&self, x1: Value, x2: Value, x3: Value, x4: Value) -> Value {
        let mut result = Value::Undefined;
        for arg in [&x1, &x2, &x3, &x4] {
            if arg.unwrap_json().is_object() {
                if result.is_undefined() || !result.unwrap_json().is_object() {
                    result = Value::Json(json!({}));
                }
                for key in arg.unwrap_json().as_object().unwrap().keys() {
                    let result1 = result.clone();
                    let val = arg.get(key.into());//self.deep_extend_2(
                    //     if result1.contains_key(key.into()) { result1.get(key.into()) } else { Value::Undefined },
                    //     arg.get(key.into()),
                    // );
                    if !val.is_undefined() {
                        result.unwrap_json_mut().as_object_mut().unwrap().insert(
                            key.to_owned(), val.unwrap_json().clone(),
                        );
                    }
                }
            }
        }
        result
    }

    fn in_array(&self, needle: Value, haystack: Value) -> Value {
        match haystack {
            Value::Json(x) if x.is_array() => x.as_array().unwrap().contains(&needle.unwrap_json()).into(),
            _ => panic!("haystack is not an array"),
        }
    }

    fn omit_zero(&self, string_number: Value) -> Value {
        if string_number.is_falsy() { Value::Undefined } else { string_number }
    }

    fn omit(&self, x: Value, keys: Value) -> Value {
        match x {
            Value::Json(x1) => {
                match x1 {
                    serde_json::Value::Object(x2) => {
                        let mut result = serde_json::Map::new();
                        for key in x2.keys() {
                            if !keys.contains_key(key.into()) {
                                result.insert(key.to_owned(), x2.get(key.into()).unwrap().clone());
                            }
                        }
                        Value::Json(serde_json::Value::Object(result))
                    }
                    _ => x1.clone().into()
                }
            }
            _ => panic!("x is not Json"),
        }
    }

    fn group_by(&self, array: Value, key: Value, out: Value) -> Value {
        let mut result = serde_json::Map::new();
        let to_array = self.to_array(array);
        let array = to_array.unwrap_json().as_array().unwrap();
        for entry in array {
            if !entry.is_object() {
                // XXX why?
                continue;
            }

            if let Some(item) = entry.as_object().unwrap().get(key.unwrap_str()) {
                if !item.is_null() {
                    let item_as_str = item.as_str().unwrap();
                    if !result.contains_key(item_as_str) {
                        result.insert(item_as_str.to_owned(), json!([]));
                    }
                    result.get_mut(item_as_str).unwrap().as_array_mut().unwrap().push(entry.clone());
                }
            }
        }
        serde_json::Value::Object(result).into()
    }

    fn safe_string(&self, x: Value, key: Value, default_value: Value) -> Value {
        if key.is_undefined() {
            return default_value;
        }
        let rv = match x {
            Value::Json(j) => match j {
                serde_json::Value::Object(o) => {
                    match o.get(key.unwrap_str()) {
                        Some(v) if v.is_string() => Value::Json(v.clone()),
                        Some(v) => Value::Json(v.to_string().into()),
                        _ => Value::Undefined
                    }
                }
                serde_json::Value::Array(a) => {
                    let index = key.unwrap_usize();
                    if index >= a.len() {
                        Value::Undefined
                    } else {
                        Value::Json(a[index].clone())
                    }
                }
                _ => Value::Undefined
            },
            _ => Value::Undefined
        };

        return if rv.is_undefined() {
            default_value
        } else {
            rv
        };
    }

    fn msec(&self) -> Value {
        SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis().to_u64().unwrap().into()
    }

    fn usec(&self) -> Value {
        SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_micros().to_u64().unwrap().into()
    }

    fn seconds(&self) -> Value {
        SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs().to_u64().unwrap().into()
    }

    fn milliseconds(&self) -> Value {
        SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis().to_u64().unwrap().into()
    }

    fn microseconds(&self) -> Value {
        SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_micros().to_u64().unwrap().into()
    }

    fn safe_string_lower(&self, x: Value, key: Value, default_value: Value) -> Value {
        let rv = match x {
            Value::Json(j) => match j {
                serde_json::Value::Object(o) => {
                    match o.get(key.unwrap_str()) {
                        Some(v) if v.is_string() => v.to_string().to_lowercase().into(),
                        _ => Value::Undefined
                    }
                }
                _ => Value::Undefined
            },
            _ => Value::Undefined
        };

        return if rv.is_undefined() {
            default_value
        } else {
            rv
        };
    }

    fn safe_string_upper(&self, x: Value, key: Value, default_value: Value) -> Value {
        let rv = match x {
            Value::Json(j) => match j {
                serde_json::Value::Object(o) => {
                    match o.get(key.unwrap_str()) {
                        Some(v) if v.is_string() => v.to_string().to_uppercase().into(),
                        _ => Value::Undefined
                    }
                }
                _ => Value::Undefined
            },
            _ => Value::Undefined
        };

        return if rv.is_undefined() {
            default_value
        } else {
            rv
        };
    }

    fn safe_integer(&self, x: Value, key: Value, default_value: Value) -> Value {
        let rv = match self.safe_value(x, key, Value::Undefined) {
            Value::Json(j) => match j {
                serde_json::Value::Number(o) => {
                    Value::Json(serde_json::Value::from(o.to_string().parse::<i64>().unwrap()))
                }
                _ => Value::Undefined
            },
            _ => Value::Undefined
        };

        if rv.is_undefined() {
            match default_value {
                Value::Json(j) => Value::Json(j),
                _ => Value::Undefined
            }
        } else {
            rv
        }
    }

    fn safe_value(&self, x: Value, key: Value, default_value: Value) -> Value {
        let rv = match x {
            Value::Json(j) => match j {
                serde_json::Value::Object(o) => {
                    match o.get(key.unwrap_str()) {
                        Some(v) => Value::Json(v.clone()),
                        _ => Value::Undefined
                    }
                }
                serde_json::Value::Array(o) => {
                    match o.get(key.unwrap_usize()) {
                        Some(v) => Value::Json(v.clone()),
                        _ => Value::Undefined
                    }
                }
                _ => Value::Undefined
            },
            _ => Value::Undefined
        };

        if rv.is_undefined() {
            match default_value {
                Value::Json(j) => Value::Json(j),
                _ => Value::Undefined
            }
        } else {
            rv
        }
    }

    fn safe_value_2(&self, x: Value, key1: Value, key2: Value, default_value: Value) -> Value {
        self.safe_value(x.clone(), key1, Value::Undefined).or_default(
            self.safe_value(x, key2, default_value))
    }

    fn safe_string_2(&self, x: Value, key1: Value, key2: Value, default_value: Value) -> Value {
        self.safe_string(x.clone(), key1, Value::Undefined).or_default(
            self.safe_string(x, key2, default_value))
    }

    fn safe_string_lower_2(&self, x: Value, key1: Value, key2: Value, default_value: Value) -> Value {
        self.safe_string_lower(x.clone(), key1, Value::Undefined).or_default(
            self.safe_string_lower(x, key2, default_value))
    }

    fn safe_string_upper_2(&self, x: Value, key1: Value, key2: Value, default_value: Value) -> Value {
        self.safe_string_upper(x.clone(), key1, Value::Undefined).or_default(
            self.safe_string_upper(x, key2, default_value))
    }

    fn keysort(&self, dictionary: Value, out: Value) -> Value {
        let obj = dictionary.unwrap_json().as_object().unwrap();
        let mut keys = obj.keys().into_iter().collect::<Vec<_>>();
        keys.sort();
        let mut result = serde_json::Map::new();
        for k in keys {
            result.insert(k.clone(), obj.get(k).unwrap().clone());
        }
        Value::Json(result.into())
    }

    fn index_by(&self, array: Value, key: Value, out: Value) -> Value {
        let mut result: serde_json::Map<String, serde_json::Value> = Default::default();
        let mut array = array.unwrap_json();
        let mut temp = serde_json::Value::Array(vec![]);
        if array.is_object() {
            let sorted = self.keysort(array.clone().into(), Value::Undefined);
            let values = sorted.unwrap_json().as_object().unwrap().values().into_iter().collect::<Vec<_>>();
            temp = serde_json::Value::Array(values.into_iter().map(|x| x.to_owned()).collect());
            array = &temp;
        }
        let is_int_key = key.unwrap_json().is_u64();
        for element in array.as_array().unwrap() {
            let element = normalize(&Value::Json(element.clone())).unwrap();
            if key.is_string() && key.unwrap_str() == "symbol" {
                // println!("key={:?}, element={:?}", key, element);
            }
            if (is_int_key && element.is_array() && (key < element.as_array().unwrap().len().into())) || (element.is_object() && element.as_object().unwrap().contains_key(key.unwrap_str())) {
                let k = if element.is_array() {
                    element.as_array().unwrap()[key.unwrap_json().as_u64().unwrap() as usize].clone()
                } else {
                    element.as_object().unwrap().get(key.unwrap_str()).unwrap().clone()
                };

                if !k.is_null() {
                    let k = normalize(&k.into()).unwrap();
                    result.insert(k.as_str().unwrap().to_owned(), element.clone());
                }
            }
        }
        Value::Json(serde_json::Value::Object(result))
    }

    fn sort_by(&self, array: Value, key: Value, descending: Value, direction: Value) -> Value {
        let descending = descending.or_default(false.into());
        let direction = direction.or_default(if descending.is_truthy() { -1 } else { 1 }.into());
        let mut array = array.unwrap_json().as_array().unwrap().clone();
        if key.is_number() {
            array.sort_by_key(|x| x.get(key.unwrap_usize()).map(|x| {
                let y: Value = x.clone().into();
                y
            }).unwrap_or("".into()));
        } else {
            array.sort_by_key(|x| x.get(key.unwrap_str()).map(|x| {
                let y: Value = x.clone().into();
                y
            }).unwrap_or("".into()));
        }
        if descending.unwrap_bool() {
            array.reverse();
        }
        Value::Json(serde_json::Value::Array(array))
    }

    fn sort_by_2(&self, array: Value, key1: Value, key2: Value, descending: Value, direction: Value) -> Value {
        let descending = descending.or_default(false.into());
        let direction = direction.or_default(if descending.is_truthy() { -1 } else { 1 }.into());
        let mut array = array.unwrap_json().as_array().unwrap().clone();
        if key1.is_number() && key2.is_number() {
            array.sort_by_key(|x| x.get(key1.unwrap_usize()).map(Into::<Value>::into).unwrap_or(
                x.get(key2.unwrap_usize()).map(Into::<Value>::into).unwrap_or("".into())).clone());
        } else if key1.is_number() {
            array.sort_by_key(|x| x.get(key1.unwrap_usize()).map(Into::<Value>::into).unwrap_or(
                x.get(key2.unwrap_str()).map(Into::<Value>::into).unwrap_or("".into())).clone());
        } else if key2.is_number() {
            array.sort_by_key(|x| x.get(key1.unwrap_str()).map(Into::<Value>::into).unwrap_or(
                x.get(key2.unwrap_usize()).map(Into::<Value>::into).unwrap_or("".into())).clone());
        } else {
            array.sort_by_key(|x| x.get(key1.unwrap_str()).map(Into::<Value>::into).unwrap_or(
                x.get(key2.unwrap_str()).map(Into::<Value>::into).unwrap_or("".into())).clone());
        }
        if descending.unwrap_bool() {
            array.reverse();
        }
        Value::Json(serde_json::Value::Array(array))
    }

    fn array_concat(&self, a: Value, b: Value) -> Value {
        let mut array = a.unwrap_json().as_array().unwrap().clone();
        array.extend(b.unwrap_json().as_array().unwrap().clone());
        Value::Json(serde_json::Value::Array(array))
    }

    fn is_empty(&self, object: Value) -> Value {
        let object = object.unwrap_json();
        if object.is_object() {
            Value::Json(serde_json::Value::Bool(object.as_object().unwrap().is_empty()))
        } else if object.is_array() {
            Value::Json(serde_json::Value::Bool(object.as_array().unwrap().is_empty()))
        } else {
            Value::Json(serde_json::Value::Bool(false))
        }
    }

    fn parse_transaction(&self, mut transaction: Value, mut currency: Value) -> Value { todo!() }
    fn parse_transfer(&self, mut transfer: Value, mut currency: Value) -> Value { todo!() }
    fn parse_market_leverage_tiers(&self, info: Value, market: Value) -> Value { todo!() }
    fn sign(&self, path: Value, api: Value, method: Value, params: Value, headers: Value, body: Value) -> Value { todo!() }
    fn yymmdd(&self, timestamp: Value, infix: Value) -> Value { todo!() }
    fn yyyymmdd(&self, timestamp: Value, infix: Value) -> Value { todo!() }
    fn ymdhms(&self, timestamp: Value, infix: Value) -> Value { todo!() }
    fn ymd(&self, timestamp: Value, infix: Value, full_year: Value) -> Value { todo!() }
    fn mdy(&self, timestamp: Value, infix: Value) -> Value { todo!() }
    async fn fetch_accounts(&self, parmas: Value) -> Value { todo!() }
    fn is_array(&self, value: Value) -> Value { todo!() }

    fn precision_from_string(&self, string: Value) -> Value {
        let re = Regex::new("0+$").unwrap();
        let len = re.replace(&string.unwrap_str(), "").split(".").collect::<Vec<_>>().len();
        if len > 1 { len } else { 0 }.into()
    }

    fn uuid22(&self, length: Value) -> Value {
        let mut arr = Vec::<u8>::new();
        arr.resize(11, 0);
        rand::thread_rng().fill_bytes(&mut arr);
        hex::encode(arr).into()
    }

    fn filter_by(&self, array: Value, key: Value, value: Value, out: Value) -> Value { todo!() }
    fn parse8601(&self, value: Value) -> Value { todo!() }
    fn rawencode(&self, value: Value) -> Value { todo!() }
    fn urlencode_with_array_repeat(&self, value: Value) -> Value { todo!() }

    fn decimal_to_precision(&self, n: Value, rounding_mode: Value, precision: Value, counting_mode: Value, padding_mode: Value) -> Value {
        // FIXME not fully implemented
        n.to_string()
    }

    fn number_to_string(&self, x: Value) -> Value { todo!() }
    async fn fetch_trades(&self, symbol: Value, since: Value, limit: Value, params: Value) -> Value { todo!() }

    fn urlencode(&self, object: Value) -> Value {
        match object {
            Value::Json(json) if json.is_object() => {
                let mut rv = String::new();
                for (key, value) in json.as_object().unwrap().iter() {
                    rv.push_str(key);
                    rv.push_str("=");
                    if value.is_string() {
                        rv.push_str(&urlencoding::encode(value.as_str().unwrap()));
                    } else{
                        rv.push_str(&urlencoding::encode(&value.to_string()));
                    }
                    rv.push_str("&");
                }
                if rv.len() > 0 {
                    rv.pop();
                }
                Value::Json(serde_json::Value::String(rv))
            }
            _ => unimplemented!()
        }
    }

    fn json(&self, data: Value, params: Value) -> Value {
        match data {
            Value::Json(json) => Value::Json(format!("{}", json).into()),
            _ => unimplemented!()
        }
    }

    fn hash(&self, request: Value, hash: Value, digest: Value) -> Value {
        let hash = hash.or_default("md5".into());
        let digest = digest.or_default("hex".into());

        let request_bytes = request.unwrap_str().as_bytes();

        let hash = match hash.unwrap_str() {
            "keccak" => sha3::Keccak256::digest(request_bytes).to_vec(),
            "sha256" => sha2::Sha256::digest(request_bytes).to_vec(),
            "sha384" => sha2::Sha384::digest(request_bytes).to_vec(),
            "sha512" => sha2::Sha512::digest(request_bytes).to_vec(),
            "md5" => md5::Md5::digest(request_bytes).to_vec(),
            _ => unimplemented!()
        };

        Value::Json(serde_json::Value::String(match digest.unwrap_str() {
            "hex" => hex::encode(hash),
            "base64" => base64::encode(hash),
            _ => panic!("unsupported digest")
        }))
    }

    fn hmac(&self, request: Value, secret: Value, hash: Value, digest: Value) -> Value {
        let hash = hash.or_default("sha256".into());
        let digest = digest.or_default("hex".into());

        let secret_bytes = secret.unwrap_str().as_bytes();
        let request_bytes = request.unwrap_str().as_bytes();

        let mac_bytes = match hash.unwrap_str() {
            "sha256" => {
                let mut mac = hmac::Hmac::<sha2::Sha256>::new_from_slice(secret_bytes).unwrap();
                mac.update(request_bytes);
                mac.finalize().into_bytes().to_vec()
            },
            "sha384" => {
                let mut mac = hmac::Hmac::<sha2::Sha384>::new_from_slice(secret_bytes).unwrap();
                mac.update(request_bytes);
                mac.finalize().into_bytes().to_vec()
            },
            "sha512" => {
                let mut mac = hmac::Hmac::<sha2::Sha512>::new_from_slice(secret_bytes).unwrap();
                mac.update(request_bytes);
                mac.finalize().into_bytes().to_vec()
            },
            _ => panic!("unsupported hash")
        };
        Value::Json(serde_json::Value::String(match digest.unwrap_str() {
            "hex" => hex::encode(mac_bytes),
            "base64" => base64::encode(mac_bytes),
            _ => panic!("unsupported digest")
        }))
    }

    fn encode(&self, x: Value) -> Value { x }

    fn parse_ticker(&self, ticker: Value, market: Value) -> Value { todo!() }
    // TODO
    fn filter_by_value_since_limit(&self, array: Value, field: Value, value: Value, since: Value, limit: Value, key: Value, tail: Value) -> Value {
        // TODO
        array
    }
    fn parse_deposit_address(&self, deposit_address: Value, currency: Value) -> Value { todo!() }
    fn parse_borrow_interest(&self, info: Value, market: Value) -> Value { todo!() }
    fn parse_funding_rate_history(&self, info: Value, market: Value) -> Value { todo!() }
    // TODO
    fn totp(&self, key: Value) -> Value { todo!() }
    fn parse_trading_limits(&self, limits: Value, symbol: Value, params: Value) -> Value { todo!() }
    fn parse_trade(&self, trade: Value, market: Value) -> Value { todo!() }
    fn parse_ledger_entry(&self, item: Value, currency: Value) -> Value { todo!() }
    fn parse_position(&self, position: Value, market: Value) -> Value { todo!() }
    // TODO
    fn implode_params(&self, string: Value, params: Value) -> Value { todo!() }
    // TODO
    fn extract_params(&self, params: Value) -> Value { todo!() }
    async fn fetch_trading_limits_by_id(&self, id: Value, params: Value) -> Value { todo!() }
    // TODO
    fn filter_by_since_limit(&self, array: Value, since: Value, limit: Value, key: Value, tail: Value) -> Value { todo!() }
    // TODO
    fn aggregate(&self, bidasks: Value) -> Value { todo!() }
    fn parse_order(&self, order: Value, market: Value) -> Value { todo!() }

    async fn fetch_currencies(&mut self, mut params: Value) -> Value { todo!() }
    async fn fetch_markets(&mut self, mut params: Value) -> Value { todo!() }

    fn iso8601(&self, timestamp: Value) -> Value {
        match timestamp {
            Value::Json(serde_json::Value::Number(x)) => {
                let x = x.as_f64().unwrap();
                if x.is_sign_negative() {
                    return Value::Undefined
                }

                let nt = NaiveDateTime::from_timestamp(
                    (x / 1000.0).floor() as i64, ((x * 1e6).floor() as u64 % 1e9 as u64) as u32
                );
                let t: DateTime<Utc> = DateTime::from_utc(nt, Utc);
                t.format("%+").to_string().into()
            }
            _ => Value::Undefined
        }
    }
    // fn fetch_borrow_rate(&self, code: Value, params: Value) -> Value { todo!() }

    // async fn load_markets(&mut self, reload: Value, params: Value) -> Value {
    //     if !reload.unwrap_bool() {
    //         if self.get("markets".into()).is_truthy() {
    //             if self.get("markets_by_id".into()).is_falsy() {
    //                 return self.set_markets(self.get("markets".into()).clone(), Value::Undefined);
    //             }
    //             return self.get("markets".into()).clone();
    //         }
    //     }
    //
    //     let mut currencies = Value::Undefined;
    //     if self.get("has".into()).get(Value::from("fetchCurrencies")).is_truthy() {
    //         unimplemented!();
    //         // currencies = self.fetch_currencies();
    //     }
    //     let markets = self.fetch_markets(currencies.clone());
    //     return self.set_markets(markets, currencies.clone());
    // }

    async fn fetch_time(&self, params: Value) -> Value { todo!() }
    fn safe_string_n(&self, dictionary: Value, key_list: Value, default_value: Value) -> Value { todo!() }
    async fn fetch_funding_rates(&self, symbols: Value, params: Value) -> Value { todo!() }
    async fn fetch_leverage_tiers(&self, symbols: Value, params: Value) -> Value { todo!() }
    fn build_ohlcvc(&self, trades: Value, timeframe: Value, since: Value, limit: Value) -> Value { todo!() }
    async fn throttle(&self, cost: Value) -> Value { todo!() }
    fn safe_timestamp(&self, dictionary: Value, key: Value, default_value: Value) -> Value { todo!() }
    fn safe_timestamp_2(&self, dictionary: Value, key1: Value, key2: Value, default_value: Value) -> Value { todo!() }
    fn check_address(&self, address: Value) -> Value { todo!() }
    fn safe_integer_2(&self, x: Value, key1: Value, key2: Value, default_value: Value) -> Value {
        let rv = self.safe_integer(x.clone(), key1, Value::Undefined);
        if rv.is_undefined() {
            self.safe_integer(x, key2, default_value)
        } else {
            rv
        }
    }
    fn parse_timeframe(&self, timeframe: Value) -> Value { todo!() }
    fn sum(&self, a: Value, b: Value) -> Value { todo!() }

    async fn fetch_deposit_addresses(&self, codes: Value, params: Value) -> Value { todo!() }
    async fn fetch_borrow_rates(&self, params: Value) -> Value { todo!() }
    async fn fetch_order_book(&self, symbol: Value, limit: Value, params: Value) -> Value { todo!() }
    async fn fetch_trading_limits(&self, symbols: Value, params: Value) -> Value { todo!() }

    async fn fetch(&self, url: Value, method: Value, headers: Value, mut body: Value) -> Value {
        let method = method.or_default("GET".into());
        let verbose: bool = self.get("verbose".into()).into();
        let client = reqwest::Client::new();
        let mut req = client.request(
            reqwest::Method::from_str(method.unwrap_str()).unwrap(),
            url.unwrap_str()
        );

        if headers.is_object() {
            let keys = headers.keys();
            for k in headers.keys() {
                let v = headers.get(k.clone());
                if v.is_string() {
                    req = req.header(k.unwrap_str(), v.unwrap_str());
                } else {
                    req = req.header(k.unwrap_str(), v.to_string().unwrap_str());
                }
            }
        }

        if !body.is_undefined() {
            req = req.body(body.unwrap_str().to_owned());
        }

        let response = req.send().await.unwrap();
        let text = response.text().await.unwrap();
        Value::Json(serde_json::Value::from_str(text.as_str()).unwrap())
    }

    fn set_sandbox_mode(&mut self, enabled: bool) {
        let mut urls = self.get("urls".into());
        if enabled {
            let api = urls.get("api".into());
            urls.set("apiBackup".into(), api);
            urls.set("api".into(), urls.get("test".into()));
            self.set("urls".into(), urls);
        } else {
            urls.set("api".into(), urls.get("apiBackup".into()));
            urls.set("apiBackup".into(), Value::Undefined);
            self.set("urls".into(), urls);
        }
    }

    // METHODS BELOW THIS LINE ARE TRANSPILED FROM JAVASCRIPT TO PYTHON AND PHP
    fn safe_ledger_entry(&self, mut entry: Value, mut currency: Value) -> Value {
        currency = self.safe_currency(Value::Undefined, currency.clone());
        let mut direction: Value = self.safe_string(entry.clone(), Value::from("direction"), Value::Undefined);
        let mut before: Value = self.safe_string(entry.clone(), Value::from("before"), Value::Undefined);
        let mut after: Value = self.safe_string(entry.clone(), Value::from("after"), Value::Undefined);
        let mut amount: Value = self.safe_string(entry.clone(), Value::from("amount"), Value::Undefined);
        if amount.clone().is_nonnullish() {
            if before.clone().is_nullish() && after.clone().is_nonnullish() {
                before = Precise::string_sub(after.clone(), amount.clone());
            } else if before.clone().is_nonnullish() && after.clone().is_nullish() {
                after = Precise::string_add(before.clone(), amount.clone());
            };
        };
        if before.clone().is_nonnullish() && after.clone().is_nonnullish() {
            if direction.clone().is_nullish() {
                if Precise::string_gt(before.clone(), after.clone()) {
                    direction = Value::from("out");
                };
                if Precise::string_gt(after.clone(), before.clone()) {
                    direction = Value::from("in");
                };
            };
        };
        let mut fee: Value = self.safe_value(entry.clone(), Value::from("fee"), Value::Undefined);
        if fee.clone().is_nonnullish() {
            fee.set("cost".into(), self.safe_number(fee.clone(), Value::from("cost"), Value::Undefined));
        };
        let mut timestamp: Value = self.safe_integer(entry.clone(), Value::from("timestamp"), Value::Undefined);
        return Value::Json(normalize(&Value::Json(json!({
            "id": self.safe_string(entry.clone(), Value::from("id"), Value::Undefined),
            "timestamp": timestamp,
            "datetime": self.iso8601(timestamp.clone()),
            "direction": direction,
            "account": self.safe_string(entry.clone(), Value::from("account"), Value::Undefined),
            "referenceId": self.safe_string(entry.clone(), Value::from("referenceId"), Value::Undefined),
            "referenceAccount": self.safe_string(entry.clone(), Value::from("referenceAccount"), Value::Undefined),
            "type": self.safe_string(entry.clone(), Value::from("type"), Value::Undefined),
            "currency": currency.get(Value::from("code")),
            "amount": self.parse_number(amount.clone(), Value::Undefined),
            "before": self.parse_number(before.clone(), Value::Undefined),
            "after": self.parse_number(after.clone(), Value::Undefined),
            "status": self.safe_string(entry.clone(), Value::from("status"), Value::Undefined),
            "fee": fee,
            "info": entry
        }))).unwrap());
    }

    fn set_markets(&mut self, mut markets: Value, mut currencies: Value) -> Value {
        let mut values: Value = Value::new_array();
        let mut market_values: Value = self.to_array(markets.clone());
        let mut i: usize = 0;
        while i < market_values.len() {
            let mut market: Value = self.deep_extend_4(self.safe_market(Value::Undefined, Value::Undefined, Value::Undefined), Value::Json(normalize(&Value::Json(json!({
                "precision": self.get("precision".into()),
                "limits": self.get("limits".into())
            }))).unwrap()), self.get("fees".into()).get(Value::from("trading")), market_values.get(i.into()));
            values.push(market.clone());
            i += 1;
        };
        self.set("markets".into(), self.index_by(values.clone(), Value::from("symbol"), Value::Undefined));
        self.set("markets_by_id".into(), self.index_by(markets.clone(), Value::from("id"), Value::Undefined));
        let mut markets_sorted_by_symbol: Value = self.keysort(self.get("markets".into()), Value::Undefined);
        let mut markets_sorted_by_id: Value = self.keysort(self.get("markets_by_id".into()), Value::Undefined);
        self.set("symbols".into(), Object::keys(markets_sorted_by_symbol.clone()));
        self.set("ids".into(), Object::keys(markets_sorted_by_id.clone()));
        if currencies.clone().is_nonnullish() {
            self.set("currencies".into(), self.deep_extend_2(self.get("currencies".into()), currencies.clone()));
        } else {
            let mut base_currencies: Value = Value::new_array();
            let mut quote_currencies: Value = Value::new_array();
            let mut i: usize = 0;
            while i < values.len() {
                let mut market: Value = values.get(i.into());
                let mut default_currency_precision: Value = if self.get("precision_mode".into()) == DECIMAL_PLACES.into() { Value::from(8) } else { self.parse_number(Value::from("0.00000001"), Value::Undefined) };
                let mut market_precision: Value = self.safe_value(market.clone(), Value::from("precision"), Value::new_object());
                if market.contains_key(Value::from("base")) {
                    let mut currency_precision: Value = self.safe_value_2(market_precision.clone(), Value::from("base"), Value::from("amount"), default_currency_precision.clone());
                    let mut currency: Value = Value::Json(normalize(&Value::Json(json!({
                        "id": self.safe_string_2(market.clone(), Value::from("baseId"), Value::from("base"), Value::Undefined),
                        "numericId": self.safe_string(market.clone(), Value::from("baseNumericId"), Value::Undefined),
                        "code": self.safe_string(market.clone(), Value::from("base"), Value::Undefined),
                        "precision": currency_precision
                    }))).unwrap());
                    base_currencies.push(currency.clone());
                };
                if market.contains_key(Value::from("quote")) {
                    let mut currency_precision: Value = self.safe_value_2(market_precision.clone(), Value::from("quote"), Value::from("amount"), default_currency_precision.clone());
                    let mut currency: Value = Value::Json(normalize(&Value::Json(json!({
                        "id": self.safe_string_2(market.clone(), Value::from("quoteId"), Value::from("quote"), Value::Undefined),
                        "numericId": self.safe_string(market.clone(), Value::from("quoteNumericId"), Value::Undefined),
                        "code": self.safe_string(market.clone(), Value::from("quote"), Value::Undefined),
                        "precision": currency_precision
                    }))).unwrap());
                    quote_currencies.push(currency.clone());
                };
                i += 1;
            };
            base_currencies = self.sort_by(base_currencies.clone(), Value::from("code"), Value::Undefined, Value::Undefined);
            quote_currencies = self.sort_by(quote_currencies.clone(), Value::from("code"), Value::Undefined, Value::Undefined);
            self.set("base_currencies".into(), self.index_by(base_currencies.clone(), Value::from("code"), Value::Undefined));
            self.set("quote_currencies".into(), self.index_by(quote_currencies.clone(), Value::from("code"), Value::Undefined));
            let mut all_currencies: Value = self.array_concat(base_currencies.clone(), quote_currencies.clone());
            let mut grouped_currencies: Value = self.group_by(all_currencies.clone(), Value::from("code"), Value::Undefined);
            let mut codes: Value = Object::keys(grouped_currencies.clone());
            let mut resulting_currencies: Value = Value::new_array();
            let mut i: usize = 0;
            while i < codes.len() {
                let mut code: Value = codes.get(i.into());
                let mut grouped_currencies_code: Value = self.safe_value(grouped_currencies.clone(), code.clone(), Value::new_array());
                let mut highest_precision_currency: Value = self.safe_value(grouped_currencies_code.clone(), Value::from(0), Value::Undefined);
                let mut j: usize = 1;
                while j < grouped_currencies_code.len() {
                    let mut current_currency: Value = grouped_currencies_code.get(j.into());
                    if self.get("precision_mode".into()) == TICK_SIZE.into() {
                        highest_precision_currency = if current_currency.get(Value::from("precision")) < highest_precision_currency.get(Value::from("precision")) { current_currency.clone() } else { highest_precision_currency.clone() };
                    } else {
                        highest_precision_currency = if current_currency.get(Value::from("precision")) > highest_precision_currency.get(Value::from("precision")) { current_currency.clone() } else { highest_precision_currency.clone() };
                    };
                    j += 1;
                };
                resulting_currencies.push(highest_precision_currency.clone());
                i += 1;
            };
            let mut sorted_currencies: Value = self.sort_by(resulting_currencies.clone(), Value::from("code"), Value::Undefined, Value::Undefined);
            self.set("currencies".into(), self.deep_extend_2(self.get("currencies".into()), self.index_by(sorted_currencies.clone(), Value::from("code"), Value::Undefined)));
        };
        self.set("currencies_by_id".into(), self.index_by(self.get("currencies".into()), Value::from("id"), Value::Undefined));
        let mut currencies_sorted_by_code: Value = self.keysort(self.get("currencies".into()), Value::Undefined);
        self.set("codes".into(), Object::keys(currencies_sorted_by_code.clone()));
        return self.get("markets".into());
    }

    fn safe_balance(&self, mut balance: Value) -> Value {
        let mut balances: Value = self.omit(balance.clone(), Value::Json(serde_json::Value::Array(vec![Value::from("info").into(), Value::from("timestamp").into(), Value::from("datetime").into(), Value::from("free").into(), Value::from("used").into(), Value::from("total").into()])));
        let mut codes: Value = Object::keys(balances.clone());
        balance.set("free".into(), Value::new_object());
        balance.set("used".into(), Value::new_object());
        balance.set("total".into(), Value::new_object());
        let mut i: usize = 0;
        while i < codes.len() {
            let mut code: Value = codes.get(i.into());
            let mut total: Value = self.safe_string(balance.get(code.clone()), Value::from("total"), Value::Undefined);
            let mut free: Value = self.safe_string(balance.get(code.clone()), Value::from("free"), Value::Undefined);
            let mut used: Value = self.safe_string(balance.get(code.clone()), Value::from("used"), Value::Undefined);
            if total.clone().is_nullish() && free.clone().is_nonnullish() && used.clone().is_nonnullish() {
                total = Precise::string_add(free.clone(), used.clone());
            };
            if free.clone().is_nullish() && total.clone().is_nonnullish() && used.clone().is_nonnullish() {
                free = Precise::string_sub(total.clone(), used.clone());
            };
            if used.clone().is_nullish() && total.clone().is_nonnullish() && free.clone().is_nonnullish() {
                used = Precise::string_sub(total.clone(), free.clone());
            };
            balance.get(code.clone()).set("free".into(), self.parse_number(free.clone(), Value::Undefined));
            balance.get(code.clone()).set("used".into(), self.parse_number(used.clone(), Value::Undefined));
            balance.get(code.clone()).set("total".into(), self.parse_number(total.clone(), Value::Undefined));
            balance.get(Value::from("free")).set(code.clone(), balance.get(code.clone()).get(Value::from("free")));
            balance.get(Value::from("used")).set(code.clone(), balance.get(code.clone()).get(Value::from("used")));
            balance.get(Value::from("total")).set(code.clone(), balance.get(code.clone()).get(Value::from("total")));
            i += 1;
        };
        return balance.clone();
    }

    fn safe_order(&mut self, mut order: Value, mut market: Value) -> Value {
        // parses numbers as strings
        // it is important pass the trades as unparsed rawTrades
        let mut amount: Value = self.omit_zero(self.safe_string(order.clone(), Value::from("amount"), Value::Undefined));
        let mut remaining: Value = self.safe_string(order.clone(), Value::from("remaining"), Value::Undefined);
        let mut filled: Value = self.safe_string(order.clone(), Value::from("filled"), Value::Undefined);
        let mut cost: Value = self.safe_string(order.clone(), Value::from("cost"), Value::Undefined);
        let mut average: Value = self.omit_zero(self.safe_string(order.clone(), Value::from("average"), Value::Undefined));
        let mut price: Value = self.omit_zero(self.safe_string(order.clone(), Value::from("price"), Value::Undefined));
        let mut last_trade_time_timestamp: Value = self.safe_integer(order.clone(), Value::from("lastTradeTimestamp"), Value::Undefined);
        let mut parse_filled: Value = (filled.clone().is_nullish()).into();
        let mut parse_cost: Value = (cost.clone().is_nullish()).into();
        let mut parse_last_trade_time_timestamp: Value = (last_trade_time_timestamp.clone().is_nullish()).into();
        let mut fee: Value = self.safe_value(order.clone(), Value::from("fee"), Value::Undefined);
        let mut parse_fee: Value = (fee.clone().is_nullish()).into();
        let mut parse_fees: Value = (self.safe_value(order.clone(), Value::from("fees"), Value::Undefined).is_nullish()).into();
        let mut should_parse_fees: Value = (parse_fee.is_truthy() || parse_fees.is_truthy()).into();
        let mut fees: Value = self.safe_value(order.clone(), Value::from("fees"), Value::new_array());
        let mut trades: Value = Value::new_array();
        if parse_filled.is_truthy() || parse_cost.is_truthy() || should_parse_fees.is_truthy() {
            let mut raw_trades: Value = self.safe_value(order.clone(), Value::from("trades"), trades.clone());
            let mut old_number: Value = self.get("number".into());
            // we parse trades as strings here!
            self.set_number_mode("String".into());
            trades = self.parse_trades(raw_trades.clone(), market.clone(), Value::Undefined, Value::Undefined, Value::Json(normalize(&Value::Json(json!({
                "symbol": order.get(Value::from("symbol")),
                "side": order.get(Value::from("side")),
                "type": order.get(Value::from("type")),
                "order": order.get(Value::from("id"))
            }))).unwrap()));
            self.set("number".into(), old_number.clone());
            let mut trades_length: Value = Value::from(0);
            let mut is_array: Value = Array::is_array(trades.clone());
            if is_array.is_truthy() {
                trades_length = trades.len().into();
            };
            if is_array.is_truthy() && trades_length.clone() > Value::from(0) {
                // move properties that are defined in trades up into the order
                if order.get(Value::from("symbol")).is_nullish() {
                    order.set("symbol".into(), trades.get(Value::from(0)).get(Value::from("symbol")));
                };
                if order.get(Value::from("side")).is_nullish() {
                    order.set("side".into(), trades.get(Value::from(0)).get(Value::from("side")));
                };
                if order.get(Value::from("type")).is_nullish() {
                    order.set("type".into(), trades.get(Value::from(0)).get(Value::from("type")));
                };
                if order.get(Value::from("id")).is_nullish() {
                    order.set("id".into(), trades.get(Value::from(0)).get(Value::from("order")));
                };
                if parse_filled.is_truthy() {
                    filled = Value::from("0");
                };
                if parse_cost.is_truthy() {
                    cost = Value::from("0");
                };
                let mut i: usize = 0;
                while i < trades.len() {
                    let mut trade: Value = trades.get(i.into());
                    let mut trade_amount: Value = self.safe_string(trade.clone(), Value::from("amount"), Value::Undefined);
                    if parse_filled.is_truthy() && trade_amount.clone().is_nonnullish() {
                        filled = Precise::string_add(filled.clone(), trade_amount.clone());
                    };
                    let mut trade_cost: Value = self.safe_string(trade.clone(), Value::from("cost"), Value::Undefined);
                    if parse_cost.is_truthy() && trade_cost.clone().is_nonnullish() {
                        cost = Precise::string_add(cost.clone(), trade_cost.clone());
                    };
                    let mut trade_timestamp: Value = self.safe_value(trade.clone(), Value::from("timestamp"), Value::Undefined);
                    if parse_last_trade_time_timestamp.is_truthy() && trade_timestamp.clone().is_nonnullish() {
                        if last_trade_time_timestamp.clone().is_nullish() {
                            last_trade_time_timestamp = trade_timestamp.clone();
                        } else {
                            last_trade_time_timestamp = Math::max(last_trade_time_timestamp.clone(), trade_timestamp.clone());
                        };
                    };
                    if should_parse_fees.is_truthy() {
                        let mut trade_fees: Value = self.safe_value(trade.clone(), Value::from("fees"), Value::Undefined);
                        if trade_fees.clone().is_nonnullish() {
                            let mut j: usize = 0;
                            while j < trade_fees.len() {
                                let mut trade_fee: Value = trade_fees.get(j.into());
                                fees.push(extend_2(Value::new_object(), trade_fee.clone()));
                                j += 1;
                            };
                        } else {
                            let mut trade_fee: Value = self.safe_value(trade.clone(), Value::from("fee"), Value::Undefined);
                            if trade_fee.clone().is_nonnullish() {
                                fees.push(extend_2(Value::new_object(), trade_fee.clone()));
                            };
                        };
                    };
                    i += 1;
                };
            };
        };
        if should_parse_fees.is_truthy() {
            let mut reduced_fees: Value = if self.get("reduce_fees".into()).is_truthy() { self.reduce_fees_by_currency(fees.clone()) } else { fees.clone() };
            let mut reduced_length: Value = reduced_fees.len().into();
            let mut i: usize = 0;
            while i < reduced_length.clone().into() {
                reduced_fees.get(i.into()).set("cost".into(), self.safe_number(reduced_fees.get(i.into()), Value::from("cost"), Value::Undefined));
                if reduced_fees.get(i.into()).contains_key(Value::from("rate")) {
                    reduced_fees.get(i.into()).set("rate".into(), self.safe_number(reduced_fees.get(i.into()), Value::from("rate"), Value::Undefined));
                };
                i += 1;
            };
            if !parse_fee.is_truthy() && reduced_length.clone() == Value::from(0) {
                fee.set("cost".into(), self.safe_number(fee.clone(), Value::from("cost"), Value::Undefined));
                if fee.contains_key(Value::from("rate")) {
                    fee.set("rate".into(), self.safe_number(fee.clone(), Value::from("rate"), Value::Undefined));
                };
                reduced_fees.push(fee.clone());
            };
            order.set("fees".into(), reduced_fees.clone());
            if parse_fee.is_truthy() && reduced_length.clone() == Value::from(1) {
                order.set("fee".into(), reduced_fees.get(Value::from(0)));
            };
        };
        if amount.clone().is_nullish() {
            // ensure amount = filled + remaining
            if filled.clone().is_nonnullish() && remaining.clone().is_nonnullish() {
                amount = Precise::string_add(filled.clone(), remaining.clone());
            } else if self.safe_string(order.clone(), Value::from("status"), Value::Undefined) == Value::from("closed") {
                amount = filled.clone();
            };
        };
        if filled.clone().is_nullish() {
            if amount.clone().is_nonnullish() && remaining.clone().is_nonnullish() {
                filled = Precise::string_sub(amount.clone(), remaining.clone());
            };
        };
        if remaining.clone().is_nullish() {
            if amount.clone().is_nonnullish() && filled.clone().is_nonnullish() {
                remaining = Precise::string_sub(amount.clone(), filled.clone());
            };
        };
        // ensure that the average field is calculated correctly
        if average.clone().is_nullish() {
            if filled.clone().is_nonnullish() && cost.clone().is_nonnullish() && Precise::string_gt(filled.clone(), Value::from("0")) {
                average = Precise::string_div(cost.clone(), filled.clone(), Value::Undefined);
            };
        };
        // also ensure the cost field is calculated correctly
        let mut cost_price_exists: Value = (average.clone().is_nonnullish() || price.clone().is_nonnullish()).into();
        if parse_cost.is_truthy() && filled.clone().is_nonnullish() && cost_price_exists.is_truthy() {
            let mut multiply_price: Value = Value::Undefined;
            if average.clone().is_nullish() {
                multiply_price = price.clone();
            } else {
                multiply_price = average.clone();
            };
            // contract trading
            let mut contract_size: Value = self.safe_string(market.clone(), Value::from("contractSize"), Value::Undefined);
            if contract_size.clone().is_nonnullish() {
                let mut inverse: Value = self.safe_value(market.clone(), Value::from("inverse"), false.into());
                if inverse.is_truthy() {
                    multiply_price = Precise::string_div(Value::from("1"), multiply_price.clone(), Value::Undefined);
                };
                multiply_price = Precise::string_mul(multiply_price.clone(), contract_size.clone());
            };
            cost = Precise::string_mul(multiply_price.clone(), filled.clone());
        };
        // support for market orders
        let mut order_type: Value = self.safe_value(order.clone(), Value::from("type"), Value::Undefined);
        let mut empty_price: Value = (price.clone().is_nullish() || Precise::string_equals(price.clone(), Value::from("0"))).into();
        if empty_price.is_truthy() && order_type.clone() == Value::from("market") {
            price = average.clone();
        };
        // we have trades with string values at this point so we will mutate them
        let mut i: usize = 0;
        while i < trades.len() {
            let mut entry: Value = trades.get(i.into());
            entry.set("amount".into(), self.safe_number(entry.clone(), Value::from("amount"), Value::Undefined));
            entry.set("price".into(), self.safe_number(entry.clone(), Value::from("price"), Value::Undefined));
            entry.set("cost".into(), self.safe_number(entry.clone(), Value::from("cost"), Value::Undefined));
            let mut fee: Value = self.safe_value(entry.clone(), Value::from("fee"), Value::new_object());
            fee.set("cost".into(), self.safe_number(fee.clone(), Value::from("cost"), Value::Undefined));
            if fee.contains_key(Value::from("rate")) {
                fee.set("rate".into(), self.safe_number(fee.clone(), Value::from("rate"), Value::Undefined));
            };
            entry.set("fee".into(), fee.clone());
            i += 1;
        };
        // timeInForceHandling
        let mut time_in_force: Value = self.safe_string(order.clone(), Value::from("timeInForce"), Value::Undefined);
        if time_in_force.clone().is_nullish() {
            if self.safe_string(order.clone(), Value::from("type"), Value::Undefined) == Value::from("market") {
                time_in_force = Value::from("IOC");
            };
            // allow postOnly override
            if self.safe_value(order.clone(), Value::from("postOnly"), false.into()).is_truthy() {
                time_in_force = Value::from("PO");
            };
        };
        return extend_2(order.clone(), Value::Json(normalize(&Value::Json(json!({
            "lastTradeTimestamp": last_trade_time_timestamp,
            "price": self.parse_number(price.clone(), Value::Undefined),
            "amount": self.parse_number(amount.clone(), Value::Undefined),
            "cost": self.parse_number(cost.clone(), Value::Undefined),
            "average": self.parse_number(average.clone(), Value::Undefined),
            "filled": self.parse_number(filled.clone(), Value::Undefined),
            "remaining": self.parse_number(remaining.clone(), Value::Undefined),
            "timeInForce": time_in_force,
            "trades": trades
        }))).unwrap()));
    }

    fn parse_orders(&mut self, mut orders: Value, mut market: Value, mut since: Value, mut limit: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        //
        // the value of orders is either a dict or a list
        //
        // dict
        //
        //     {
        //         'id1': { ... },
        //         'id2': { ... },
        //         'id3': { ... },
        //         ...
        //     }
        //
        // list
        //
        //     [
        //         { 'id': 'id1', ... },
        //         { 'id': 'id2', ... },
        //         { 'id': 'id3', ... },
        //         ...
        //     ]
        //
        let mut results: Value = Value::new_array();
        if Array::is_array(orders.clone()).is_truthy() {
            let mut i: usize = 0;
            while i < orders.len() {
                let mut order: Value = extend_2(self.parse_order(orders.get(i.into()), market.clone()), params.clone());
                results.push(order.clone());
                i += 1;
            };
        } else {
            let mut ids: Value = Object::keys(orders.clone());
            let mut i: usize = 0;
            while i < ids.len() {
                let mut id: Value = ids.get(i.into());
                let mut order: Value = extend_2(self.parse_order(extend_2(Value::Json(normalize(&Value::Json(json!({
                    "id": id
                }))).unwrap()), orders.get(id.clone())), market.clone()), params.clone());
                results.push(order.clone());
                i += 1;
            };
        };
        results = self.sort_by(results.clone(), Value::from("timestamp"), Value::Undefined, Value::Undefined);
        let mut symbol: Value = if market.clone().is_nonnullish() { market.get(Value::from("symbol")) } else { Value::Undefined };
        let mut tail: Value = (since.clone().is_nullish()).into();
        return self.filter_by_symbol_since_limit(results.clone(), symbol.clone(), since.clone(), limit.clone(), tail.clone());
    }

    fn calculate_fee(&mut self, mut symbol: Value, mut r#type: Value, mut side: Value, mut amount: Value, mut price: Value, mut taker_or_maker: Value, mut params: Value) -> Value {
        taker_or_maker = taker_or_maker.or_default(Value::from("taker"));
        params = params.or_default(Value::new_object());
        let mut market: Value = self.get("markets".into()).get(symbol.clone());
        let mut fee_side: Value = self.safe_string(market.clone(), Value::from("feeSide"), Value::from("quote"));
        let mut key: Value = Value::from("quote");
        let mut cost: Value = Value::Undefined;
        if fee_side.clone() == Value::from("quote") {
            // the fee is always in quote currency
            cost = amount.clone() * price.clone();
        } else if fee_side.clone() == Value::from("base") {
            // the fee is always in base currency
            cost = amount.clone();
        } else if fee_side.clone() == Value::from("get") {
            // the fee is always in the currency you get
            cost = amount.clone();
            if side.clone() == Value::from("sell") {
                cost = cost *  price.clone();
            } else {
                key = Value::from("base");
            };
        } else if fee_side.clone() == Value::from("give") {
            // the fee is always in the currency you give
            cost = amount.clone();
            if side.clone() == Value::from("buy") {
                cost = cost *  price.clone();
            } else {
                key = Value::from("base");
            };
        };
        let mut rate: Value = market.get(taker_or_maker.clone());
        if cost.clone().is_nonnullish() {
            cost = cost *  rate.clone();
        };
        return Value::Json(normalize(&Value::Json(json!({
            "type": taker_or_maker,
            "currency": market.get(key.clone()),
            "rate": rate,
            "cost": cost
        }))).unwrap());
    }

    fn safe_trade(&mut self, mut trade: Value, mut market: Value) -> Value {
        let mut amount: Value = self.safe_string(trade.clone(), Value::from("amount"), Value::Undefined);
        let mut price: Value = self.safe_string(trade.clone(), Value::from("price"), Value::Undefined);
        let mut cost: Value = self.safe_string(trade.clone(), Value::from("cost"), Value::Undefined);
        if cost.clone().is_nullish() {
            // contract trading
            let mut contract_size: Value = self.safe_string(market.clone(), Value::from("contractSize"), Value::Undefined);
            let mut multiply_price: Value = price.clone();
            if contract_size.clone().is_nonnullish() {
                let mut inverse: Value = self.safe_value(market.clone(), Value::from("inverse"), false.into());
                if inverse.is_truthy() {
                    multiply_price = Precise::string_div(Value::from("1"), price.clone(), Value::Undefined);
                };
                multiply_price = Precise::string_mul(multiply_price.clone(), contract_size.clone());
            };
            cost = Precise::string_mul(multiply_price.clone(), amount.clone());
        };
        let mut parse_fee: Value = (self.safe_value(trade.clone(), Value::from("fee"), Value::Undefined).is_nullish()).into();
        let mut parse_fees: Value = (self.safe_value(trade.clone(), Value::from("fees"), Value::Undefined).is_nullish()).into();
        let mut should_parse_fees: Value = (parse_fee.is_truthy() || parse_fees.is_truthy()).into();
        let mut fees: Value = Value::new_array();
        if should_parse_fees.is_truthy() {
            let mut trade_fees: Value = self.safe_value(trade.clone(), Value::from("fees"), Value::Undefined);
            if trade_fees.clone().is_nonnullish() {
                let mut j: usize = 0;
                while j < trade_fees.len() {
                    let mut trade_fee: Value = trade_fees.get(j.into());
                    fees.push(extend_2(Value::new_object(), trade_fee.clone()));
                    j += 1;
                };
            } else {
                let mut trade_fee: Value = self.safe_value(trade.clone(), Value::from("fee"), Value::Undefined);
                if trade_fee.clone().is_nonnullish() {
                    fees.push(extend_2(Value::new_object(), trade_fee.clone()));
                };
            };
        };
        let mut fee: Value = self.safe_value(trade.clone(), Value::from("fee"), Value::Undefined);
        if should_parse_fees.is_truthy() {
            let mut reduced_fees: Value = if self.get("reduce_fees".into()).is_truthy() { self.reduce_fees_by_currency(fees.clone()) } else { fees.clone() };
            let mut reduced_length: Value = reduced_fees.len().into();
            let mut i: usize = 0;
            while i < reduced_length.clone().into() {
                reduced_fees.get(i.into()).set("cost".into(), self.safe_number(reduced_fees.get(i.into()), Value::from("cost"), Value::Undefined));
                if reduced_fees.get(i.into()).contains_key(Value::from("rate")) {
                    reduced_fees.get(i.into()).set("rate".into(), self.safe_number(reduced_fees.get(i.into()), Value::from("rate"), Value::Undefined));
                };
                i += 1;
            };
            if !parse_fee.is_truthy() && reduced_length.clone() == Value::from(0) {
                fee.set("cost".into(), self.safe_number(fee.clone(), Value::from("cost"), Value::Undefined));
                if fee.contains_key(Value::from("rate")) {
                    fee.set("rate".into(), self.safe_number(fee.clone(), Value::from("rate"), Value::Undefined));
                };
                reduced_fees.push(fee.clone());
            };
            if parse_fees.is_truthy() {
                trade.set("fees".into(), reduced_fees.clone());
            };
            if parse_fee.is_truthy() && reduced_length.clone() == Value::from(1) {
                trade.set("fee".into(), reduced_fees.get(Value::from(0)));
            };
            let mut trade_fee: Value = self.safe_value(trade.clone(), Value::from("fee"), Value::Undefined);
            if trade_fee.clone().is_nonnullish() {
                trade_fee.set("cost".into(), self.safe_number(trade_fee.clone(), Value::from("cost"), Value::Undefined));
                if trade_fee.contains_key(Value::from("rate")) {
                    trade_fee.set("rate".into(), self.safe_number(trade_fee.clone(), Value::from("rate"), Value::Undefined));
                };
                trade.set("fee".into(), trade_fee.clone());
            };
        };
        trade.set("amount".into(), self.parse_number(amount.clone(), Value::Undefined));
        trade.set("price".into(), self.parse_number(price.clone(), Value::Undefined));
        trade.set("cost".into(), self.parse_number(cost.clone(), Value::Undefined));
        return trade.clone();
    }

    fn reduce_fees_by_currency(&mut self, mut fees: Value) -> Value {
        //
        // this function takes a list of fee structures having the following format
        //
        //     string = true
        //
        //     [
        //         { 'currency': 'BTC', 'cost': '0.1' },
        //         { 'currency': 'BTC', 'cost': '0.2'  },
        //         { 'currency': 'BTC', 'cost': '0.2', 'rate': '0.00123' },
        //         { 'currency': 'BTC', 'cost': '0.4', 'rate': '0.00123' },
        //         { 'currency': 'BTC', 'cost': '0.5', 'rate': '0.00456' },
        //         { 'currency': 'USDT', 'cost': '12.3456' },
        //     ]
        //
        //     string = false
        //
        //     [
        //         { 'currency': 'BTC', 'cost': 0.1 },
        //         { 'currency': 'BTC', 'cost': 0.2 },
        //         { 'currency': 'BTC', 'cost': 0.2, 'rate': 0.00123 },
        //         { 'currency': 'BTC', 'cost': 0.4, 'rate': 0.00123 },
        //         { 'currency': 'BTC', 'cost': 0.5, 'rate': 0.00456 },
        //         { 'currency': 'USDT', 'cost': 12.3456 },
        //     ]
        //
        // and returns a reduced fee list, where fees are summed per currency and rate (if any)
        //
        //     string = true
        //
        //     [
        //         { 'currency': 'BTC', 'cost': '0.3'  },
        //         { 'currency': 'BTC', 'cost': '0.6', 'rate': '0.00123' },
        //         { 'currency': 'BTC', 'cost': '0.5', 'rate': '0.00456' },
        //         { 'currency': 'USDT', 'cost': '12.3456' },
        //     ]
        //
        //     string  = false
        //
        //     [
        //         { 'currency': 'BTC', 'cost': 0.3  },
        //         { 'currency': 'BTC', 'cost': 0.6, 'rate': 0.00123 },
        //         { 'currency': 'BTC', 'cost': 0.5, 'rate': 0.00456 },
        //         { 'currency': 'USDT', 'cost': 12.3456 },
        //     ]
        //
        let mut reduced: Value = Value::new_object();
        let mut i: usize = 0;
        while i < fees.len() {
            let mut fee: Value = fees.get(i.into());
            let mut fee_currency_code: Value = self.safe_string(fee.clone(), Value::from("currency"), Value::Undefined);
            if fee_currency_code.clone().is_nonnullish() {
                let mut rate: Value = self.safe_string(fee.clone(), Value::from("rate"), Value::Undefined);
                let mut cost: Value = self.safe_value(fee.clone(), Value::from("cost"), Value::Undefined);
                if Precise::string_eq(cost.clone(), Value::from("0")) {
                    // omit zero cost fees
                    continue;
                };
                if !reduced.contains_key(fee_currency_code.clone()) {
                    reduced.set(fee_currency_code.clone(), Value::new_object());
                };
                let mut rate_key: Value = if rate.clone().is_nullish() { Value::from("") } else { rate.clone() };
                if reduced.get(fee_currency_code.clone()).contains_key(rate_key.clone()) {
                    reduced.get(fee_currency_code.clone()).get(rate_key.clone()).set("cost".into(), Precise::string_add(reduced.get(fee_currency_code.clone()).get(rate_key.clone()).get(Value::from("cost")), cost.clone()));
                } else {
                    reduced.get(fee_currency_code.clone()).set(rate_key.clone(), Value::Json(normalize(&Value::Json(json!({
                        "currency": fee_currency_code,
                        "cost": cost
                    }))).unwrap()));
                    if rate.clone().is_nonnullish() {
                        reduced.get(fee_currency_code.clone()).get(rate_key.clone()).set("rate".into(), rate.clone());
                    };
                };
            };
            i += 1;
        };
        let mut result: Value = Value::new_array();
        let mut fee_values: Value = Object::values(reduced.clone());
        let mut i: usize = 0;
        while i < fee_values.len() {
            let mut reduced_fee_values: Value = Object::values(fee_values.get(i.into()));
            result = self.array_concat(result.clone(), reduced_fee_values.clone());
            i += 1;
        };
        return result.clone();
    }

    fn safe_ticker(&self, mut ticker: Value, mut market: Value) -> Value {
        let mut open: Value = self.safe_value(ticker.clone(), Value::from("open"), Value::Undefined);
        let mut close: Value = self.safe_value(ticker.clone(), Value::from("close"), Value::Undefined);
        let mut last: Value = self.safe_value(ticker.clone(), Value::from("last"), Value::Undefined);
        let mut change: Value = self.safe_value(ticker.clone(), Value::from("change"), Value::Undefined);
        let mut percentage: Value = self.safe_value(ticker.clone(), Value::from("percentage"), Value::Undefined);
        let mut average: Value = self.safe_value(ticker.clone(), Value::from("average"), Value::Undefined);
        let mut vwap: Value = self.safe_value(ticker.clone(), Value::from("vwap"), Value::Undefined);
        let mut base_volume: Value = self.safe_value(ticker.clone(), Value::from("baseVolume"), Value::Undefined);
        let mut quote_volume: Value = self.safe_value(ticker.clone(), Value::from("quoteVolume"), Value::Undefined);
        if vwap.clone().is_nullish() {
            vwap = Precise::string_div(quote_volume.clone(), base_volume.clone(), Value::Undefined);
        };
        if last.clone().is_nonnullish() && close.clone().is_nullish() {
            close = last.clone();
        } else if last.clone().is_nullish() && close.clone().is_nonnullish() {
            last = close.clone();
        };
        if last.clone().is_nonnullish() && open.clone().is_nonnullish() {
            if change.clone().is_nullish() {
                change = Precise::string_sub(last.clone(), open.clone());
            };
            if average.clone().is_nullish() {
                average = Precise::string_div(Precise::string_add(last.clone(), open.clone()), Value::from("2"), Value::Undefined);
            };
        };
        if percentage.clone().is_nullish() && change.clone().is_nonnullish() && open.clone().is_nonnullish() && Precise::string_gt(open.clone(), Value::from("0")) {
            percentage = Precise::string_mul(Precise::string_div(change.clone(), open.clone(), Value::Undefined), Value::from("100"));
        };
        if change.clone().is_nullish() && percentage.clone().is_nonnullish() && open.clone().is_nonnullish() {
            change = Precise::string_div(Precise::string_mul(percentage.clone(), open.clone()), Value::from("100"), Value::Undefined);
        };
        if open.clone().is_nullish() && last.clone().is_nonnullish() && change.clone().is_nonnullish() {
            open = Precise::string_sub(last.clone(), change.clone());
        };
        // timestamp and symbol operations don't belong in safeTicker
        // they should be done in the derived classes
        return extend_2(ticker.clone(), Value::Json(normalize(&Value::Json(json!({
            "bid": self.safe_number(ticker.clone(), Value::from("bid"), Value::Undefined),
            "bidVolume": self.safe_number(ticker.clone(), Value::from("bidVolume"), Value::Undefined),
            "ask": self.safe_number(ticker.clone(), Value::from("ask"), Value::Undefined),
            "askVolume": self.safe_number(ticker.clone(), Value::from("askVolume"), Value::Undefined),
            "high": self.safe_number(ticker.clone(), Value::from("high"), Value::Undefined),
            "low": self.safe_number(ticker.clone(), Value::from("low"), Value::Undefined),
            "open": self.parse_number(open.clone(), Value::Undefined),
            "close": self.parse_number(close.clone(), Value::Undefined),
            "last": self.parse_number(last.clone(), Value::Undefined),
            "change": self.parse_number(change.clone(), Value::Undefined),
            "percentage": self.parse_number(percentage.clone(), Value::Undefined),
            "average": self.parse_number(average.clone(), Value::Undefined),
            "vwap": self.parse_number(vwap.clone(), Value::Undefined),
            "baseVolume": self.parse_number(base_volume.clone(), Value::Undefined),
            "quoteVolume": self.parse_number(quote_volume.clone(), Value::Undefined),
            "previousClose": self.safe_number(ticker.clone(), Value::from("previousClose"), Value::Undefined)
        }))).unwrap()));
    }

    async fn fetch_ohlcv(&mut self, mut symbol: Value, mut timeframe: Value, mut since: Value, mut limit: Value, mut params: Value) -> Value {
        timeframe = timeframe.or_default(Value::from("1m"));
        params = params.or_default(Value::new_object());
        if !self.get("has".into()).get(Value::from("fetchTrades")).is_truthy() {
            panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchOHLCV() is not supported yet"))"###);
        };
        self.load_markets(Value::Undefined, Value::Undefined).await;
        let mut trades: Value = self.fetch_trades(symbol.clone(), since.clone(), limit.clone(), params.clone()).await;
        let mut ohlcvc: Value = self.build_ohlcvc(trades.clone(), timeframe.clone(), since.clone(), limit.clone());
        let mut result: Value = Value::new_array();
        let mut i: usize = 0;
        while i < ohlcvc.len() {
            result.push(Value::Json(serde_json::Value::Array(vec![self.safe_integer(ohlcvc.get(i.into()), Value::from(0), Value::Undefined).into(), self.safe_number(ohlcvc.get(i.into()), Value::from(1), Value::Undefined).into(), self.safe_number(ohlcvc.get(i.into()), Value::from(2), Value::Undefined).into(), self.safe_number(ohlcvc.get(i.into()), Value::from(3), Value::Undefined).into(), self.safe_number(ohlcvc.get(i.into()), Value::from(4), Value::Undefined).into(), self.safe_number(ohlcvc.get(i.into()), Value::from(5), Value::Undefined).into()])));
            i += 1;
        };
        return result.clone();
    }

    fn convert_trading_view_to_ohlcv(&self, mut ohlcvs: Value, mut timestamp: Value, mut open: Value, mut high: Value, mut low: Value, mut close: Value, mut volume: Value, mut ms: Value) -> Value {
        timestamp = timestamp.or_default(Value::from("t"));
        open = open.or_default(Value::from("o"));
        high = high.or_default(Value::from("h"));
        low = low.or_default(Value::from("l"));
        close = close.or_default(Value::from("c"));
        volume = volume.or_default(Value::from("v"));
        ms = ms.or_default(false.into());
        let mut result: Value = Value::new_array();
        let mut timestamps: Value = self.safe_value(ohlcvs.clone(), timestamp.clone(), Value::new_array());
        let mut opens: Value = self.safe_value(ohlcvs.clone(), open.clone(), Value::new_array());
        let mut highs: Value = self.safe_value(ohlcvs.clone(), high.clone(), Value::new_array());
        let mut lows: Value = self.safe_value(ohlcvs.clone(), low.clone(), Value::new_array());
        let mut closes: Value = self.safe_value(ohlcvs.clone(), close.clone(), Value::new_array());
        let mut volumes: Value = self.safe_value(ohlcvs.clone(), volume.clone(), Value::new_array());
        let mut i: usize = 0;
        while i < timestamps.len() {
            result.push(Value::Json(serde_json::Value::Array(vec![if ms.is_truthy() { self.safe_integer(timestamps.clone(), Value::from(i), Value::Undefined) } else { self.safe_timestamp(timestamps.clone(), Value::from(i), Value::Undefined) }.into(), self.safe_value(opens.clone(), Value::from(i), Value::Undefined).into(), self.safe_value(highs.clone(), Value::from(i), Value::Undefined).into(), self.safe_value(lows.clone(), Value::from(i), Value::Undefined).into(), self.safe_value(closes.clone(), Value::from(i), Value::Undefined).into(), self.safe_value(volumes.clone(), Value::from(i), Value::Undefined).into()])));
            i += 1;
        };
        return result.clone();
    }

    fn convert_ohlcv_to_trading_view(&self, mut ohlcvs: Value, mut timestamp: Value, mut open: Value, mut high: Value, mut low: Value, mut close: Value, mut volume: Value, mut ms: Value) -> Value {
        timestamp = timestamp.or_default(Value::from("t"));
        open = open.or_default(Value::from("o"));
        high = high.or_default(Value::from("h"));
        low = low.or_default(Value::from("l"));
        close = close.or_default(Value::from("c"));
        volume = volume.or_default(Value::from("v"));
        ms = ms.or_default(false.into());
        let mut result: Value = Value::new_object();
        result.set(timestamp.clone(), Value::new_array());
        result.set(open.clone(), Value::new_array());
        result.set(high.clone(), Value::new_array());
        result.set(low.clone(), Value::new_array());
        result.set(close.clone(), Value::new_array());
        result.set(volume.clone(), Value::new_array());
        let mut i: usize = 0;
        while i < ohlcvs.len() {
            let mut ts: Value = if ms.is_truthy() { ohlcvs.get(i.into()).get(Value::from(0)) } else { parse_int(ohlcvs.get(i.into()).get(Value::from(0)) / Value::from(1000)) };
            result.get(timestamp.clone()).push(ts.clone());
            result.get(open.clone()).push(ohlcvs.get(i.into()).get(Value::from(1)));
            result.get(high.clone()).push(ohlcvs.get(i.into()).get(Value::from(2)));
            result.get(low.clone()).push(ohlcvs.get(i.into()).get(Value::from(3)));
            result.get(close.clone()).push(ohlcvs.get(i.into()).get(Value::from(4)));
            result.get(volume.clone()).push(ohlcvs.get(i.into()).get(Value::from(5)));
            i += 1;
        };
        return result.clone();
    }

    fn market_ids(&mut self, mut symbols: Value) -> Value {
        let mut result: Value = Value::new_array();
        let mut i: usize = 0;
        while i < symbols.len() {
            result.push(self.market_id(symbols.get(i.into())));
            i += 1;
        };
        return result.clone();
    }

    fn market_symbols(&self, mut symbols: Value) -> Value {
        if symbols.clone().is_nullish() {
            return symbols.clone();
        };
        let mut result: Value = Value::new_array();
        let mut i: usize = 0;
        while i < symbols.len() {
            result.push(self.symbol(symbols.get(i.into())));
            i += 1;
        };
        return result.clone();
    }

    fn parse_bids_asks(&self, mut bidasks: Value, mut price_key: Value, mut amount_key: Value) -> Value {
        price_key = price_key.or_default(Value::from(0));
        amount_key = amount_key.or_default(Value::from(1));
        bidasks = self.to_array(bidasks.clone());
        let mut result: Value = Value::new_array();
        let mut i: usize = 0;
        while i < bidasks.len() {
            result.push(self.parse_bid_ask(bidasks.get(i.into()), price_key.clone(), amount_key.clone()));
            i += 1;
        };
        return result.clone();
    }

    async fn fetch_l2_order_book(&mut self, mut symbol: Value, mut limit: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        let mut orderbook: Value = self.fetch_order_book(symbol.clone(), limit.clone(), params.clone()).await;
        return extend_2(orderbook.clone(), Value::Json(normalize(&Value::Json(json!({
            "asks": self.sort_by(self.aggregate(orderbook.get(Value::from("asks"))), Value::from(0), Value::Undefined, Value::Undefined),
            "bids": self.sort_by(self.aggregate(orderbook.get(Value::from("bids"))), Value::from(0), true.into(), Value::Undefined)
        }))).unwrap()));
    }

    fn filter_by_symbol(&self, mut objects: Value, mut symbol: Value) -> Value {
        if symbol.clone().is_nullish() {
            return objects.clone();
        };
        let mut result: Value = Value::new_array();
        let mut i: usize = 0;
        while i < objects.len() {
            let mut object_symbol: Value = self.safe_string(objects.get(i.into()), Value::from("symbol"), Value::Undefined);
            if object_symbol.clone() == symbol.clone() {
                result.push(objects.get(i.into()));
            };
            i += 1;
        };
        return result.clone();
    }

    fn parse_ohlcv(&self, mut ohlcv: Value, mut market: Value) -> Value {
        if Array::is_array(ohlcv.clone()).is_truthy() {
            return Value::Json(serde_json::Value::Array(vec![self.safe_integer(ohlcv.clone(), Value::from(0), Value::Undefined).into(), self.safe_number(ohlcv.clone(), Value::from(1), Value::Undefined).into(), self.safe_number(ohlcv.clone(), Value::from(2), Value::Undefined).into(), self.safe_number(ohlcv.clone(), Value::from(3), Value::Undefined).into(), self.safe_number(ohlcv.clone(), Value::from(4), Value::Undefined).into(), self.safe_number(ohlcv.clone(), Value::from(5), Value::Undefined).into()]));
        };
        // timestamp
        // open
        // high
        // low
        // close
        // volume
        return ohlcv.clone();
    }

    fn get_network(&mut self, mut network: Value, mut code: Value) -> Value {
        network = network.to_upper_case();
        let mut aliases: Value = Value::Json(normalize(&Value::Json(json!({
            "ETHEREUM": "ETH",
            "ETHER": "ETH",
            "ERC20": "ETH",
            "ETH": "ETH",
            "TRC20": "TRX",
            "TRON": "TRX",
            "TRX": "TRX",
            "BEP20": "BSC",
            "BSC": "BSC",
            "HRC20": "HT",
            "HECO": "HT",
            "SPL": "SOL",
            "SOL": "SOL",
            "TERRA": "LUNA",
            "LUNA": "LUNA",
            "POLYGON": "MATIC",
            "MATIC": "MATIC",
            "EOS": "EOS",
            "WAVES": "WAVES",
            "AVALANCHE": "AVAX",
            "AVAX": "AVAX",
            "QTUM": "QTUM",
            "CHZ": "CHZ",
            "NEO": "NEO",
            "ONT": "ONT",
            "RON": "RON"
        }))).unwrap());
        if network.clone() == code.clone() {
            return network.clone();
        } else if aliases.contains_key(network.clone()) {
            return aliases.get(network.clone());
        } else {
            panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" network ") + network.clone() + Value::from(" is not yet supported"))"###);
        };
        Value::Undefined
    }

    fn safe_number_2(&self, mut dictionary: Value, mut key1: Value, mut key2: Value, mut d: Value) -> Value {
        let mut value: Value = self.safe_string_2(dictionary.clone(), key1.clone(), key2.clone(), Value::Undefined);
        return self.parse_number(value.clone(), d.clone());
    }

    fn parse_order_book(&self, mut orderbook: Value, mut symbol: Value, mut timestamp: Value, mut bids_key: Value, mut asks_key: Value, mut price_key: Value, mut amount_key: Value) -> Value {
        bids_key = bids_key.or_default(Value::from("bids"));
        asks_key = asks_key.or_default(Value::from("asks"));
        price_key = price_key.or_default(Value::from(0));
        amount_key = amount_key.or_default(Value::from(1));
        let mut bids: Value = self.parse_bids_asks(self.safe_value(orderbook.clone(), bids_key.clone(), Value::new_array()), price_key.clone(), amount_key.clone());
        let mut asks: Value = self.parse_bids_asks(self.safe_value(orderbook.clone(), asks_key.clone(), Value::new_array()), price_key.clone(), amount_key.clone());
        return Value::Json(normalize(&Value::Json(json!({
            "symbol": symbol,
            "bids": self.sort_by(bids.clone(), Value::from(0), true.into(), Value::Undefined),
            "asks": self.sort_by(asks.clone(), Value::from(0), Value::Undefined, Value::Undefined),
            "timestamp": timestamp,
            "datetime": self.iso8601(timestamp.clone()),
            "nonce": Value::Undefined
        }))).unwrap());
    }

    fn parse_ohlcvs(&self, mut ohlcvs: Value, mut market: Value, mut timeframe: Value, mut since: Value, mut limit: Value) -> Value {
        timeframe = timeframe.or_default(Value::from("1m"));
        let mut results: Value = Value::new_array();
        let mut i: usize = 0;
        while i < ohlcvs.len() {
            results.push(self.parse_ohlcv(ohlcvs.get(i.into()), market.clone()));
            i += 1;
        };
        let mut sorted: Value = self.sort_by(results.clone(), Value::from(0), Value::Undefined, Value::Undefined);
        let mut tail: Value = (since.clone().is_nullish()).into();
        return self.filter_by_since_limit(sorted.clone(), since.clone(), limit.clone(), Value::from(0), tail.clone());
    }

    fn parse_leverage_tiers(&self, mut response: Value, mut symbols: Value, mut market_id_key: Value) -> Value {
        // marketIdKey should only be undefined when response is a dictionary
        symbols = self.market_symbols(symbols.clone());
        let mut tiers: Value = Value::new_object();
        let mut i: usize = 0;
        while i < response.len() {
            let mut item: Value = response.get(i.into());
            let mut id: Value = self.safe_string(item.clone(), market_id_key.clone(), Value::Undefined);
            let mut market: Value = self.safe_market(id.clone(), Value::Undefined, Value::Undefined);
            let mut symbol: Value = market.get(Value::from("symbol"));
            let mut contract: Value = self.safe_value(market.clone(), Value::from("contract"), false.into());
            if contract.is_truthy() && symbols.clone().is_nullish() || self.in_array(symbol.clone(), symbols.clone()).is_truthy() {
                tiers.set(symbol.clone(), self.parse_market_leverage_tiers(item.clone(), market.clone()));
            };
            i += 1;
        };
        return tiers.clone();
    }

    async fn load_trading_limits(&mut self, mut symbols: Value, mut reload: Value, mut params: Value) -> Value {
        reload = reload.or_default(false.into());
        params = params.or_default(Value::new_object());
        if self.get("has".into()).get(Value::from("fetchTradingLimits")).is_truthy() {
            if reload.is_truthy() || !self.get("options".into()).contains_key(Value::from("limitsLoaded")) {
                let mut response: Value = self.fetch_trading_limits(symbols.clone(), Value::Undefined).await;
                let mut i: usize = 0;
                while i < symbols.len() {
                    let mut symbol: Value = symbols.get(i.into());
                    self.get("markets".into()).set(symbol.clone(), self.deep_extend_2(self.get("markets".into()).get(symbol.clone()), response.get(symbol.clone())));
                    i += 1;
                };
                self.get("options".into()).set("limitsLoaded".into(), self.milliseconds());
            };
        };
        return self.get("markets".into());
    }

    fn parse_positions(&self, mut positions: Value, mut symbols: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        symbols = self.market_symbols(symbols.clone());
        positions = self.to_array(positions.clone());
        let mut result: Value = Value::new_array();
        let mut i: usize = 0;
        while i < positions.len() {
            let mut position: Value = extend_2(self.parse_position(positions.get(i.into()), Value::Undefined), params.clone());
            result.push(position.clone());
            i += 1;
        };
        return self.filter_by_array(result.clone(), Value::from("symbol"), symbols.clone(), false.into());
    }

    fn parse_accounts(&self, mut accounts: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        accounts = self.to_array(accounts.clone());
        let mut result: Value = Value::new_array();
        let mut i: usize = 0;
        while i < accounts.len() {
            let mut account: Value = extend_2(self.parse_account(accounts.get(i.into())), params.clone());
            result.push(account.clone());
            i += 1;
        };
        return result.clone();
    }

    fn parse_trades(&mut self, mut trades: Value, mut market: Value, mut since: Value, mut limit: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        trades = self.to_array(trades.clone());
        let mut result: Value = Value::new_array();
        let mut i: usize = 0;
        while i < trades.len() {
            let mut trade: Value = extend_2(self.parse_trade(trades.get(i.into()), market.clone()), params.clone());
            result.push(trade.clone());
            i += 1;
        };
        result = self.sort_by_2(result.clone(), Value::from("timestamp"), Value::from("id"), Value::Undefined, Value::Undefined);
        let mut symbol: Value = if market.clone().is_nonnullish() { market.get(Value::from("symbol")) } else { Value::Undefined };
        let mut tail: Value = (since.clone().is_nullish()).into();
        return self.filter_by_symbol_since_limit(result.clone(), symbol.clone(), since.clone(), limit.clone(), tail.clone());
    }

    fn parse_transactions(&self, mut transactions: Value, mut currency: Value, mut since: Value, mut limit: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        transactions = self.to_array(transactions.clone());
        let mut result: Value = Value::new_array();
        let mut i: usize = 0;
        while i < transactions.len() {
            let mut transaction: Value = extend_2(self.parse_transaction(transactions.get(i.into()), currency.clone()), params.clone());
            result.push(transaction.clone());
            i += 1;
        };
        result = self.sort_by(result.clone(), Value::from("timestamp"), Value::Undefined, Value::Undefined);
        let mut code: Value = if currency.clone().is_nonnullish() { currency.get(Value::from("code")) } else { Value::Undefined };
        let mut tail: Value = (since.clone().is_nullish()).into();
        return self.filter_by_currency_since_limit(result.clone(), code.clone(), since.clone(), limit.clone(), tail.clone());
    }

    fn parse_transfers(&self, mut transfers: Value, mut currency: Value, mut since: Value, mut limit: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        transfers = self.to_array(transfers.clone());
        let mut result: Value = Value::new_array();
        let mut i: usize = 0;
        while i < transfers.len() {
            let mut transfer: Value = extend_2(self.parse_transfer(transfers.get(i.into()), currency.clone()), params.clone());
            result.push(transfer.clone());
            i += 1;
        };
        result = self.sort_by(result.clone(), Value::from("timestamp"), Value::Undefined, Value::Undefined);
        let mut code: Value = if currency.clone().is_nonnullish() { currency.get(Value::from("code")) } else { Value::Undefined };
        let mut tail: Value = (since.clone().is_nullish()).into();
        return self.filter_by_currency_since_limit(result.clone(), code.clone(), since.clone(), limit.clone(), tail.clone());
    }

    fn parse_ledger(&self, mut data: Value, mut currency: Value, mut since: Value, mut limit: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        let mut result: Value = Value::new_array();
        let mut array_data: Value = self.to_array(data.clone());
        let mut i: usize = 0;
        while i < array_data.len() {
            let mut item_or_items: Value = self.parse_ledger_entry(array_data.get(i.into()), currency.clone());
            if Array::is_array(item_or_items.clone()).is_truthy() {
                let mut j: usize = 0;
                while j < item_or_items.len() {
                    result.push(extend_2(item_or_items.get(j.into()), params.clone()));
                    j += 1;
                };
            } else {
                result.push(extend_2(item_or_items.clone(), params.clone()));
            };
            i += 1;
        };
        result = self.sort_by(result.clone(), Value::from("timestamp"), Value::Undefined, Value::Undefined);
        let mut code: Value = if currency.clone().is_nonnullish() { currency.get(Value::from("code")) } else { Value::Undefined };
        let mut tail: Value = (since.clone().is_nullish()).into();
        return self.filter_by_currency_since_limit(result.clone(), code.clone(), since.clone(), limit.clone(), tail.clone());
    }

    fn nonce(&self) -> Value {
        return self.seconds();
    }

    fn set_headers(&mut self, mut headers: Value) -> Value {
        return headers.clone();
    }

    fn market_id(&mut self, mut symbol: Value) -> Value {
        let mut market: Value = self.market(symbol.clone());
        if market.clone().is_nonnullish() {
            return market.get(Value::from("id"));
        };
        return symbol.clone();
    }

    fn symbol(&self, mut symbol: Value) -> Value {
        let mut market: Value = self.market(symbol.clone());
        return self.safe_string(market.clone(), Value::from("symbol"), symbol.clone());
    }

    fn resolve_path(&mut self, mut path: Value, mut params: Value) -> Value {
        return Value::Json(serde_json::Value::Array(vec![self.implode_params(path.clone(), params.clone()).into(), self.omit(params.clone(), self.extract_params(path.clone())).into()]));
    }

    fn filter_by_array(&self, mut objects: Value, mut key: Value, mut values: Value, mut indexed: Value) -> Value {
        indexed = indexed.or_default(true.into());
        objects = self.to_array(objects.clone());
        // return all of them if no values were passed
        if values.clone().is_nullish() || !values.is_truthy() {
            return if indexed.is_truthy() { self.index_by(objects.clone(), key.clone(), Value::Undefined) } else { objects.clone() };
        };
        let mut results: Value = Value::new_array();
        let mut i: usize = 0;
        while i < objects.len() {
            if self.in_array(objects.get(i.into()).get(key.clone()), values.clone()).is_truthy() {
                results.push(objects.get(i.into()));
            };
            i += 1;
        };
        return if indexed.is_truthy() { self.index_by(results.clone(), key.clone(), Value::Undefined) } else { results.clone() };
    }

    async fn fetch2(&mut self, mut path: Value, mut api: Value, mut method: Value, mut params: Value, mut headers: Value, mut body: Value, mut config: Value, mut context: Value) -> Value {
        api = api.or_default(Value::from("public"));
        method = method.or_default(Value::from("GET"));
        params = params.or_default(Value::new_object());
        config = config.or_default(Value::new_object());
        context = context.or_default(Value::new_object());
        if self.get("enable_rate_limit".into()).is_truthy() {
            let mut cost: Value = self.calculate_rate_limiter_cost(api.clone(), method.clone(), path.clone(), params.clone(), config.clone(), context.clone());
            self.throttle(cost.clone()).await;
        };
        self.set("last_rest_request_timestamp".into(), self.milliseconds());
        let mut request: Value = self.sign(path.clone(), api.clone(), method.clone(), params.clone(), headers.clone(), body.clone());
        return self.fetch(request.get(Value::from("url")), request.get(Value::from("method")), request.get(Value::from("headers")), request.get(Value::from("body"))).await;
    }

    async fn request(&mut self, mut path: Value, mut api: Value, mut method: Value, mut params: Value, mut headers: Value, mut body: Value, mut config: Value, mut context: Value) -> Value {
        api = api.or_default(Value::from("public"));
        method = method.or_default(Value::from("GET"));
        params = params.or_default(Value::new_object());
        config = config.or_default(Value::new_object());
        context = context.or_default(Value::new_object());
        return self.fetch2(path.clone(), api.clone(), method.clone(), params.clone(), headers.clone(), body.clone(), config.clone(), context.clone()).await;
    }

    async fn load_accounts(&mut self, mut reload: Value, mut params: Value) -> Value {
        reload = reload.or_default(false.into());
        params = params.or_default(Value::new_object());
        if reload.is_truthy() {
            self.set("accounts".into(), self.fetch_accounts(params.clone()).await);
        } else {
            if self.get("accounts".into()).is_truthy() {
                return self.get("accounts".into());
            } else {
                self.set("accounts".into(), self.fetch_accounts(params.clone()).await);
            };
        };
        self.set("accounts_by_id".into(), self.index_by(self.get("accounts".into()), Value::from("id"), Value::Undefined));
        return self.get("accounts".into());
    }

    async fn fetch_ohlcvc(&mut self, mut symbol: Value, mut timeframe: Value, mut since: Value, mut limit: Value, mut params: Value) -> Value {
        timeframe = timeframe.or_default(Value::from("1m"));
        params = params.or_default(Value::new_object());
        if !self.get("has".into()).get(Value::from("fetchTrades")).is_truthy() {
            panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchOHLCV() is not supported yet"))"###);
        };
        self.load_markets(Value::Undefined, Value::Undefined).await;
        let mut trades: Value = self.fetch_trades(symbol.clone(), since.clone(), limit.clone(), params.clone()).await;
        return self.build_ohlcvc(trades.clone(), timeframe.clone(), since.clone(), limit.clone());
    }

    fn parse_trading_view_ohlcv(&self, mut ohlcvs: Value, mut market: Value, mut timeframe: Value, mut since: Value, mut limit: Value) -> Value {
        timeframe = timeframe.or_default(Value::from("1m"));
        let mut result: Value = self.convert_trading_view_to_ohlcv(ohlcvs.clone(), Value::Undefined, Value::Undefined, Value::Undefined, Value::Undefined, Value::Undefined, Value::Undefined, Value::Undefined);
        return self.parse_ohlcvs(result.clone(), market.clone(), timeframe.clone(), since.clone(), limit.clone());
    }

    async fn edit_limit_buy_order(&mut self, mut id: Value, mut symbol: Value, mut amount: Value, mut price: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        return self.edit_limit_order(id.clone(), symbol.clone(), Value::from("buy"), amount.clone(), price.clone(), params.clone()).await;
    }

    async fn edit_limit_sell_order(&mut self, mut id: Value, mut symbol: Value, mut amount: Value, mut price: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        return self.edit_limit_order(id.clone(), symbol.clone(), Value::from("sell"), amount.clone(), price.clone(), params.clone()).await;
    }

    async fn edit_limit_order(&mut self, mut id: Value, mut symbol: Value, mut side: Value, mut amount: Value, mut price: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        return self.edit_order(id.clone(), symbol.clone(), Value::from("limit"), side.clone(), amount.clone(), price.clone(), params.clone()).await;
    }

    async fn edit_order(&mut self, mut id: Value, mut symbol: Value, mut r#type: Value, mut side: Value, mut amount: Value, mut price: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        self.cancel_order(id.clone(), symbol.clone(), Value::Undefined).await;
        return self.create_order(symbol.clone(), r#type.clone(), side.clone(), amount.clone(), price.clone(), params.clone()).await;
    }

    async fn fetch_permissions(&mut self, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchPermissions() is not supported yet"))"###);
        Value::Undefined
    }

    async fn fetch_bids_asks(&mut self, mut symbols: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchBidsAsks() is not supported yet"))"###);
        Value::Undefined
    }

    fn parse_bid_ask(&self, mut bidask: Value, mut price_key: Value, mut amount_key: Value) -> Value {
        price_key = price_key.or_default(Value::from(0));
        amount_key = amount_key.or_default(Value::from(1));
        let mut price: Value = self.safe_number(bidask.clone(), price_key.clone(), Value::Undefined);
        let mut amount: Value = self.safe_number(bidask.clone(), amount_key.clone(), Value::Undefined);
        return Value::Json(serde_json::Value::Array(vec![price.clone().into(), amount.clone().into()]));
    }

    fn safe_currency(&self, mut currency_id: Value, mut currency: Value) -> Value {
        if currency_id.clone().is_nullish() && currency.clone().is_nonnullish() {
            return currency.clone();
        };
        if self.get("currencies_by_id".into()).is_nonnullish() && self.get("currencies_by_id".into()).contains_key(currency_id.clone()) {
            return self.get("currencies_by_id".into()).get(currency_id.clone());
        };
        let mut code: Value = currency_id.clone();
        if currency_id.clone().is_nonnullish() {
            code = self.common_currency_code(currency_id.to_upper_case());
        };
        return Value::Json(normalize(&Value::Json(json!({
            "id": currency_id,
            "code": code
        }))).unwrap());
    }

    fn safe_market(&self, mut market_id: Value, mut market: Value, mut delimiter: Value) -> Value {
        let mut result: Value = Value::Json(normalize(&Value::Json(json!({
            "id": market_id,
            "symbol": market_id,
            "base": Value::Undefined,
            "quote": Value::Undefined,
            "baseId": Value::Undefined,
            "quoteId": Value::Undefined,
            "active": Value::Undefined,
            "type": Value::Undefined,
            "linear": Value::Undefined,
            "inverse": Value::Undefined,
            "spot": false,
            "swap": false,
            "future": false,
            "option": false,
            "margin": false,
            "contract": false,
            "contractSize": Value::Undefined,
            "expiry": Value::Undefined,
            "expiryDatetime": Value::Undefined,
            "optionType": Value::Undefined,
            "strike": Value::Undefined,
            "settle": Value::Undefined,
            "settleId": Value::Undefined,
            "precision": Value::Json(normalize(&Value::Json(json!({
                "amount": Value::Undefined,
                "price": Value::Undefined
            }))).unwrap()),
            "limits": Value::Json(normalize(&Value::Json(json!({
                "amount": Value::Json(normalize(&Value::Json(json!({
                    "min": Value::Undefined,
                    "max": Value::Undefined
                }))).unwrap()),
                "price": Value::Json(normalize(&Value::Json(json!({
                    "min": Value::Undefined,
                    "max": Value::Undefined
                }))).unwrap()),
                "cost": Value::Json(normalize(&Value::Json(json!({
                    "min": Value::Undefined,
                    "max": Value::Undefined
                }))).unwrap())
            }))).unwrap()),
            "info": Value::Undefined
        }))).unwrap());
        if market_id.clone().is_nonnullish() {
            if self.get("markets_by_id".into()).is_nonnullish() && self.get("markets_by_id".into()).contains_key(market_id.clone()) {
                market = self.get("markets_by_id".into()).get(market_id.clone());
            } else if delimiter.clone().is_nonnullish() {
                let mut parts: Value = market_id.split(delimiter.clone());
                let mut parts_length: Value = parts.len().into();
                if parts_length.clone() == Value::from(2) {
                    result.set("baseId".into(), self.safe_string(parts.clone(), Value::from(0), Value::Undefined));
                    result.set("quoteId".into(), self.safe_string(parts.clone(), Value::from(1), Value::Undefined));
                    result.set("base".into(), self.safe_currency_code(result.get(Value::from("baseId")), Value::Undefined));
                    result.set("quote".into(), self.safe_currency_code(result.get(Value::from("quoteId")), Value::Undefined));
                    result.set("symbol".into(), result.get(Value::from("base")) + Value::from("/") + result.get(Value::from("quote")));
                    return result.clone();
                } else {
                    return result.clone();
                };
            };
        };
        if market.clone().is_nonnullish() {
            return market.clone();
        };
        return result.clone();
    }

    fn check_required_credentials(&mut self, mut error: Value) -> Value {
        error = error.or_default(true.into());
        let mut keys: Value = Object::keys(self.get("required_credentials".into()));
        let mut i: usize = 0;
        while i < keys.len() {
            let mut key: Value = keys.get(i.into());
            if self.get("required_credentials".into()).get(key.clone()).is_truthy() && !self.get(key.clone()).is_truthy() {
                if error.is_truthy() {
                    panic!(r###"AuthenticationError::new(self.get("id".into()) + Value::from(r#" requires ""#) + key.clone() + Value::from(r#"" credential"#))"###);
                } else {
                    return error.clone();
                };
            };
            i += 1;
        };
        return true.into();
    }

    fn oath(&mut self) -> Value {
        if self.get("twofa".into()).is_nonnullish() {
            return self.totp(self.get("twofa".into()));
        } else {
            panic!(r###"ExchangeError::new(self.get("id".into()) + Value::from(" exchange.twofa has not been set for 2FA Two-Factor Authentication"))"###);
        };
        Value::Undefined
    }

    async fn fetch_balance(&mut self, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchBalance() is not supported yet"))"###);
        Value::Undefined
    }

    async fn fetch_partial_balance(&mut self, mut part: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        let mut balance: Value = self.fetch_balance(params.clone()).await;
        return balance.get(part.clone());
    }

    async fn fetch_free_balance(&mut self, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        return self.fetch_partial_balance(Value::from("free"), params.clone()).await;
    }

    async fn fetch_used_balance(&mut self, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        return self.fetch_partial_balance(Value::from("used"), params.clone()).await;
    }

    async fn fetch_total_balance(&mut self, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        return self.fetch_partial_balance(Value::from("total"), params.clone()).await;
    }

    async fn fetch_status(&mut self, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        if self.get("has".into()).get(Value::from("fetchTime")).is_truthy() {
            let mut time: Value = self.fetch_time(params.clone()).await;
            self.set("status".into(), extend_2(self.get("status".into()), Value::Json(normalize(&Value::Json(json!({
                "updated": time
            }))).unwrap())));
        };
        return self.get("status".into());
    }

    async fn fetch_funding_fee(&mut self, mut code: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        let mut warn_on_fetch_funding_fee: Value = self.safe_value(self.get("options".into()), Value::from("warnOnFetchFundingFee"), true.into());
        if warn_on_fetch_funding_fee.is_truthy() {
            panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(r#" fetchFundingFee() method is deprecated, it will be removed in July 2022, please, use fetchTransactionFee() or set exchange.options["warnOnFetchFundingFee"] = false to suppress this warning"#))"###);
        };
        return self.fetch_transaction_fee(code.clone(), params.clone()).await;
    }

    async fn fetch_funding_fees(&mut self, mut codes: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        let mut warn_on_fetch_funding_fees: Value = self.safe_value(self.get("options".into()), Value::from("warnOnFetchFundingFees"), true.into());
        if warn_on_fetch_funding_fees.is_truthy() {
            panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(r#" fetchFundingFees() method is deprecated, it will be removed in July 2022. Please, use fetchTransactionFees() or set exchange.options["warnOnFetchFundingFees"] = false to suppress this warning"#))"###);
        };
        return self.fetch_transaction_fees(codes.clone(), params.clone()).await;
    }

    async fn fetch_transaction_fee(&mut self, mut code: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        if !self.get("has".into()).get(Value::from("fetchTransactionFees")).is_truthy() {
            panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchTransactionFee() is not supported yet"))"###);
        };
        return self.fetch_transaction_fees(Value::Json(serde_json::Value::Array(vec![code.clone().into()])), params.clone()).await;
    }

    async fn fetch_transaction_fees(&mut self, mut codes: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchTransactionFees() is not supported yet"))"###);
        Value::Undefined
    }

    fn get_supported_mapping(&self, mut key: Value, mut mapping: Value) -> Value {
        mapping = mapping.or_default(Value::new_object());
        if mapping.contains_key(key.clone()) {
            return mapping.get(key.clone());
        } else {
            panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" ") + key.clone() + Value::from(" does not have a value in mapping"))"###);
        };
        Value::Undefined
    }

    async fn fetch_borrow_rate(&mut self, mut code: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        self.load_markets(Value::Undefined, Value::Undefined).await;
        if !self.get("has".into()).get(Value::from("fetchBorrowRates")).is_truthy() {
            panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchBorrowRate() is not supported yet"))"###);
        };
        let mut borrow_rates: Value = self.fetch_borrow_rates(params.clone()).await;
        let mut rate: Value = self.safe_value(borrow_rates.clone(), code.clone(), Value::Undefined);
        if rate.clone().is_nullish() {
            panic!(r###"ExchangeError::new(self.get("id".into()) + Value::from(" fetchBorrowRate() could not find the borrow rate for currency code ") + code.clone())"###);
        };
        return rate.clone();
    }

    fn handle_market_type_and_params(&mut self, mut method_name: Value, mut market: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        let mut default_type: Value = self.safe_string_2(self.get("options".into()), Value::from("defaultType"), Value::from("type"), Value::from("spot"));
        let mut method_options: Value = self.safe_value(self.get("options".into()), method_name.clone(), Value::Undefined);
        let mut method_type: Value = default_type.clone();
        if method_options.clone().is_nonnullish() {
            if method_options.typeof_() == Value::from("string") {
                method_type = method_options.clone();
            } else {
                method_type = self.safe_string_2(method_options.clone(), Value::from("defaultType"), Value::from("type"), method_type.clone());
            };
        };
        let mut market_type: Value = if market.clone().is_nullish() { method_type.clone() } else { market.get(Value::from("type")) };
        let mut r#type: Value = self.safe_string_2(params.clone(), Value::from("defaultType"), Value::from("type"), market_type.clone());
        params = self.omit(params.clone(), Value::Json(serde_json::Value::Array(vec![Value::from("defaultType").into(), Value::from("type").into()])));
        return Value::Json(serde_json::Value::Array(vec![r#type.clone().into(), params.clone().into()]));
    }

    fn handle_sub_type_and_params(&mut self, mut method_name: Value, mut market: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        let mut sub_type: Value = Value::Undefined;
        // if set in params, it takes precedence
        let mut sub_type_in_params: Value = self.safe_string_2(params.clone(), Value::from("subType"), Value::from("subType"), Value::Undefined);
        // avoid omitting if it's not present
        if sub_type_in_params.clone().is_nonnullish() {
            sub_type = sub_type_in_params.clone();
            params = self.omit(params.clone(), Value::Json(serde_json::Value::Array(vec![Value::from("defaultSubType").into(), Value::from("subType").into()])));
        } else {
            // at first, check from market object
            if market.clone().is_nonnullish() {
                if market.get(Value::from("linear")).is_truthy() {
                    sub_type = Value::from("linear");
                } else if market.get(Value::from("inverse")).is_truthy() {
                    sub_type = Value::from("inverse");
                };
            };
            // if it was not defined in market object
            if sub_type.clone().is_nullish() {
                let mut exchange_wide_value: Value = self.safe_string_2(self.get("options".into()), Value::from("defaultSubType"), Value::from("subType"), Value::from("linear"));
                let mut method_options: Value = self.safe_value(self.get("options".into()), method_name.clone(), Value::new_object());
                sub_type = self.safe_string_2(method_options.clone(), Value::from("defaultSubType"), Value::from("subType"), exchange_wide_value.clone());
            };
        };
        return Value::Json(serde_json::Value::Array(vec![sub_type.clone().into(), params.clone().into()]));
    }

    fn throw_exactly_matched_exception(&mut self, mut exact: Value, mut string: Value, mut message: Value) -> () {
        if exact.contains_key(string.clone()) {
            panic!(r###"exact.get(string.clone())::new(message)"###);
        };
    }

    fn throw_broadly_matched_exception(&mut self, mut broad: Value, mut string: Value, mut message: Value) -> () {
        let mut broad_key: Value = self.find_broadly_matched_key(broad.clone(), string.clone());
        if broad_key.clone().is_nonnullish() {
            panic!(r###"broad.get(broad_key.clone())::new(message)"###);
        };
    }

    fn find_broadly_matched_key(&mut self, mut broad: Value, mut string: Value) -> Value {
        // a helper for matching error strings exactly vs broadly
        let mut keys: Value = Object::keys(broad.clone());
        let mut i: usize = 0;
        while i < keys.len() {
            let mut key: Value = keys.get(i.into());
            if string.index_of(key.clone()) >= Value::from(0) {
                return key.clone();
            };
            i += 1;
        };
        return Value::Undefined;
    }

    fn handle_errors(&mut self, mut status_code: Value, mut status_text: Value, mut url: Value, mut method: Value, mut response_headers: Value, mut response_body: Value, mut response: Value, mut request_headers: Value, mut request_body: Value) -> Value { Value::Undefined }

    fn calculate_rate_limiter_cost(&mut self, mut api: Value, mut method: Value, mut path: Value, mut params: Value, mut config: Value, mut context: Value) -> Value {
        config = config.or_default(Value::new_object());
        context = context.or_default(Value::new_object());
        return self.safe_value(config.clone(), Value::from("cost"), Value::from(1));
    }

    async fn fetch_ticker(&mut self, mut symbol: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        if self.get("has".into()).get(Value::from("fetchTickers")).is_truthy() {
            let mut tickers: Value = self.fetch_tickers(Value::Json(serde_json::Value::Array(vec![symbol.clone().into()])), params.clone()).await;
            let mut ticker: Value = self.safe_value(tickers.clone(), symbol.clone(), Value::Undefined);
            if ticker.clone().is_nullish() {
                panic!(r###"NullResponse::new(self.get("id".into()) + Value::from(" fetchTickers() could not find a ticker for ") + symbol.clone())"###);
            } else {
                return ticker.clone();
            };
        } else {
            panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchTicker() is not supported yet"))"###);
        };
        Value::Undefined
    }

    async fn fetch_tickers(&mut self, mut symbols: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchTickers() is not supported yet"))"###);
        Value::Undefined
    }

    async fn fetch_order(&mut self, mut id: Value, mut symbol: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchOrder() is not supported yet"))"###);
        Value::Undefined
    }

    async fn fetch_order_status(&mut self, mut id: Value, mut symbol: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        let mut order: Value = self.fetch_order(id.clone(), symbol.clone(), params.clone()).await;
        return order.get(Value::from("status"));
    }

    async fn fetch_unified_order(&mut self, mut order: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        return self.fetch_order(self.safe_value(order.clone(), Value::from("id"), Value::Undefined), self.safe_value(order.clone(), Value::from("symbol"), Value::Undefined), params.clone()).await;
    }

    async fn create_order(&mut self, mut symbol: Value, mut r#type: Value, mut side: Value, mut amount: Value, mut price: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" createOrder() is not supported yet"))"###);
        Value::Undefined
    }

    async fn cancel_order(&mut self, mut id: Value, mut symbol: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" cancelOrder() is not supported yet"))"###);
        Value::Undefined
    }

    async fn cancel_unified_order(&mut self, mut order: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        return self.cancel_order(self.safe_value(order.clone(), Value::from("id"), Value::Undefined), self.safe_value(order.clone(), Value::from("symbol"), Value::Undefined), params.clone()).await;
    }

    async fn fetch_orders(&mut self, mut symbol: Value, mut since: Value, mut limit: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchOrders() is not supported yet"))"###);
        Value::Undefined
    }

    async fn fetch_open_orders(&mut self, mut symbol: Value, mut since: Value, mut limit: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchOpenOrders() is not supported yet"))"###);
        Value::Undefined
    }

    async fn fetch_closed_orders(&mut self, mut symbol: Value, mut since: Value, mut limit: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchClosedOrders() is not supported yet"))"###);
        Value::Undefined
    }

    async fn fetch_my_trades(&mut self, mut symbol: Value, mut since: Value, mut limit: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchMyTrades() is not supported yet"))"###);
        Value::Undefined
    }

    async fn fetch_transactions(&mut self, mut symbol: Value, mut since: Value, mut limit: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchTransactions() is not supported yet"))"###);
        Value::Undefined
    }

    async fn fetch_deposits(&mut self, mut symbol: Value, mut since: Value, mut limit: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchDeposits() is not supported yet"))"###);
        Value::Undefined
    }

    async fn fetch_withdrawals(&mut self, mut symbol: Value, mut since: Value, mut limit: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchWithdrawals() is not supported yet"))"###);
        Value::Undefined
    }

    async fn fetch_deposit_address(&mut self, mut code: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        if self.get("has".into()).get(Value::from("fetchDepositAddresses")).is_truthy() {
            let mut deposit_addresses: Value = self.fetch_deposit_addresses(Value::Json(serde_json::Value::Array(vec![code.clone().into()])), params.clone()).await;
            let mut deposit_address: Value = self.safe_value(deposit_addresses.clone(), code.clone(), Value::Undefined);
            if deposit_address.clone().is_nullish() {
                panic!(r###"InvalidAddress::new(self.get("id".into()) + Value::from(" fetchDepositAddress() could not find a deposit address for ") + code.clone() + Value::from(", make sure you have created a corresponding deposit address in your wallet on the exchange website"))"###);
            } else {
                return deposit_address.clone();
            };
        } else {
            panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchDepositAddress() is not supported yet"))"###);
        };
        Value::Undefined
    }

    fn account(&self) -> Value {
        return Value::Json(normalize(&Value::Json(json!({
            "free": Value::Undefined,
            "used": Value::Undefined,
            "total": Value::Undefined
        }))).unwrap());
    }

    fn common_currency_code(&self, mut currency: Value) -> Value {
        if !self.get("substitute_common_currency_codes".into()).is_truthy() {
            return currency.clone();
        };
        return self.safe_string(self.get("common_currencies".into()), currency.clone(), currency.clone());
    }

    fn currency(&self, mut code: Value) -> Value {
        if self.get("currencies".into()).is_nullish() {
            panic!(r###"ExchangeError::new(self.get("id".into()) + Value::from(" currencies not loaded"))"###);
        };
        if code.typeof_() == Value::from("string") {
            if self.get("currencies".into()).contains_key(code.clone()) {
                return self.get("currencies".into()).get(code.clone());
            } else if self.get("currencies_by_id".into()).contains_key(code.clone()) {
                return self.get("currencies_by_id".into()).get(code.clone());
            };
        };
        panic!(r###"ExchangeError::new(self.get("id".into()) + Value::from(" does not have currency code ") + code.clone())"###);
        Value::Undefined
    }

    fn market(&self, mut symbol: Value) -> Value {
        if self.get("markets".into()).is_nullish() {
            panic!(r###"ExchangeError::new(self.get("id".into()) + Value::from(" markets not loaded"))"###);
        };
        if self.get("markets_by_id".into()).is_nullish() {
            panic!(r###"ExchangeError::new(self.get("id".into()) + Value::from(" markets not loaded"))"###);
        };
        if symbol.typeof_() == Value::from("string") {
            if self.get("markets".into()).contains_key(symbol.clone()) {
                return self.get("markets".into()).get(symbol.clone());
            } else if self.get("markets_by_id".into()).contains_key(symbol.clone()) {
                return self.get("markets_by_id".into()).get(symbol.clone());
            };
        };
        panic!(r###"BadSymbol::new(self.get("id".into()) + Value::from(" does not have market symbol ") + symbol.clone())"###);
        Value::Undefined
    }

    fn handle_withdraw_tag_and_params(&mut self, mut tag: Value, mut params: Value) -> Value {
        if tag.typeof_() == Value::from("object") {
            params = extend_2(tag.clone(), params.clone());
            tag = Value::Undefined;
        };
        if tag.clone().is_nullish() {
            tag = self.safe_string(params.clone(), Value::from("tag"), Value::Undefined);
            if tag.clone().is_nonnullish() {
                params = self.omit(params.clone(), Value::from("tag"));
            };
        };
        return Value::Json(serde_json::Value::Array(vec![tag.clone().into(), params.clone().into()]));
    }

    async fn create_limit_order(&mut self, mut symbol: Value, mut side: Value, mut amount: Value, mut price: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        return self.create_order(symbol.clone(), Value::from("limit"), side.clone(), amount.clone(), price.clone(), params.clone()).await;
    }

    async fn create_market_order(&mut self, mut symbol: Value, mut side: Value, mut amount: Value, mut price: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        return self.create_order(symbol.clone(), Value::from("market"), side.clone(), amount.clone(), price.clone(), params.clone()).await;
    }

    async fn create_limit_buy_order(&mut self, mut symbol: Value, mut amount: Value, mut price: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        return self.create_order(symbol.clone(), Value::from("limit"), Value::from("buy"), amount.clone(), price.clone(), params.clone()).await;
    }

    async fn create_limit_sell_order(&mut self, mut symbol: Value, mut amount: Value, mut price: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        return self.create_order(symbol.clone(), Value::from("limit"), Value::from("sell"), amount.clone(), price.clone(), params.clone()).await;
    }

    async fn create_market_buy_order(&mut self, mut symbol: Value, mut amount: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        return self.create_order(symbol.clone(), Value::from("market"), Value::from("buy"), amount.clone(), Value::Undefined, params.clone()).await;
    }

    async fn create_market_sell_order(&mut self, mut symbol: Value, mut amount: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        return self.create_order(symbol.clone(), Value::from("market"), Value::from("sell"), amount.clone(), Value::Undefined, params.clone()).await;
    }

    fn cost_to_precision(&mut self, mut symbol: Value, mut cost: Value) -> Value {
        let mut market: Value = self.market(symbol.clone());
        return self.decimal_to_precision(cost.clone(), TRUNCATE.into(), market.get(Value::from("precision")).get(Value::from("price")), self.get("precision_mode".into()), self.get("padding_mode".into()));
    }

    fn price_to_precision(&mut self, mut symbol: Value, mut price: Value) -> Value {
        let mut market: Value = self.market(symbol.clone());
        return self.decimal_to_precision(price.clone(), ROUND.into(), market.get(Value::from("precision")).get(Value::from("price")), self.get("precision_mode".into()), self.get("padding_mode".into()));
    }

    fn amount_to_precision(&mut self, mut symbol: Value, mut amount: Value) -> Value {
        let mut market: Value = self.market(symbol.clone());
        return self.decimal_to_precision(amount.clone(), TRUNCATE.into(), market.get(Value::from("precision")).get(Value::from("amount")), self.get("precision_mode".into()), self.get("padding_mode".into()));
    }

    fn fee_to_precision(&mut self, mut symbol: Value, mut fee: Value) -> Value {
        let mut market: Value = self.market(symbol.clone());
        return self.decimal_to_precision(fee.clone(), ROUND.into(), market.get(Value::from("precision")).get(Value::from("price")), self.get("precision_mode".into()), self.get("padding_mode".into()));
    }

    fn currency_to_precision(&mut self, mut code: Value, mut fee: Value, mut network_code: Value) -> Value {
        let mut currency: Value = self.get("currencies".into()).get(code.clone());
        let mut precision: Value = self.safe_value(currency.clone(), Value::from("precision"), Value::Undefined);
        if network_code.clone().is_nonnullish() {
            let mut networks: Value = self.safe_value(currency.clone(), Value::from("networks"), Value::new_object());
            let mut network_item: Value = self.safe_value(networks.clone(), network_code.clone(), Value::new_object());
            precision = self.safe_value(network_item.clone(), Value::from("precision"), precision.clone());
        };
        if precision.clone().is_nullish() {
            return fee.clone();
        } else {
            return self.decimal_to_precision(fee.clone(), ROUND.into(), precision.clone(), self.get("precision_mode".into()), self.get("padding_mode".into()));
        };
        Value::Undefined
    }

    fn safe_number(&self, mut object: Value, mut key: Value, mut d: Value) -> Value {
        let mut value: Value = self.safe_string(object.clone(), key.clone(), Value::Undefined);
        return self.parse_number(value.clone(), d.clone());
    }

    fn safe_number_n(&self, mut object: Value, mut arr: Value, mut d: Value) -> Value {
        let mut value: Value = self.safe_string_n(object.clone(), arr.clone(), Value::Undefined);
        return self.parse_number(value.clone(), d.clone());
    }

    fn parse_precision(&self, mut precision: Value) -> Value {
        if precision.clone().is_nullish() {
            return Value::Undefined;
        };
        return Value::from("1e") + Precise::string_neg(precision.clone());
    }

    async fn load_time_difference(&mut self, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        let mut server_time: Value = self.fetch_time(params.clone()).await;
        let mut after: Value = self.milliseconds();
        self.get("options".into()).set("timeDifference".into(), after.clone() - server_time.clone());
        return self.get("options".into()).get(Value::from("timeDifference"));
    }

    fn implode_hostname(&mut self, mut url: Value) -> Value {
        return self.implode_params(url.clone(), Value::Json(normalize(&Value::Json(json!({
            "hostname": self.get("hostname".into())
        }))).unwrap()));
    }

    async fn fetch_market_leverage_tiers(&mut self, mut symbol: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        if self.get("has".into()).get(Value::from("fetchLeverageTiers")).is_truthy() {
            let mut market: Value = self.market(symbol.clone());
            if !market.get(Value::from("contract")).is_truthy() {
                panic!(r###"BadSymbol::new(self.get("id".into()) + Value::from(" fetchMarketLeverageTiers() supports contract markets only"))"###);
            };
            let mut tiers: Value = self.fetch_leverage_tiers(Value::Json(serde_json::Value::Array(vec![symbol.clone().into()])), Value::Undefined).await;
            return self.safe_value(tiers.clone(), symbol.clone(), Value::Undefined);
        } else {
            panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchMarketLeverageTiers() is not supported yet"))"###);
        };
        Value::Undefined
    }

    async fn create_post_only_order(&mut self, mut symbol: Value, mut r#type: Value, mut side: Value, mut amount: Value, mut price: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        if !self.get("has".into()).get(Value::from("createPostOnlyOrder")).is_truthy() {
            panic!(r###"NotSupported::new(self.get("id".into()) + Value::from("createPostOnlyOrder() is not supported yet"))"###);
        };
        let mut query: Value = extend_2(params.clone(), Value::Json(normalize(&Value::Json(json!({
            "postOnly": true
        }))).unwrap()));
        return self.create_order(symbol.clone(), r#type.clone(), side.clone(), amount.clone(), price.clone(), query.clone()).await;
    }

    async fn create_reduce_only_order(&mut self, mut symbol: Value, mut r#type: Value, mut side: Value, mut amount: Value, mut price: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        if !self.get("has".into()).get(Value::from("createReduceOnlyOrder")).is_truthy() {
            panic!(r###"NotSupported::new(self.get("id".into()) + Value::from("createReduceOnlyOrder() is not supported yet"))"###);
        };
        let mut query: Value = extend_2(params.clone(), Value::Json(normalize(&Value::Json(json!({
            "reduceOnly": true
        }))).unwrap()));
        return self.create_order(symbol.clone(), r#type.clone(), side.clone(), amount.clone(), price.clone(), query.clone()).await;
    }

    async fn create_stop_order(&mut self, mut symbol: Value, mut r#type: Value, mut side: Value, mut amount: Value, mut price: Value, mut stop_price: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        if !self.get("has".into()).get(Value::from("createStopOrder")).is_truthy() {
            panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" createStopOrder() is not supported yet"))"###);
        };
        if stop_price.clone().is_nullish() {
            panic!(r###"ArgumentsRequired::new(self.get("id".into()) + Value::from(" create_stop_order() requires a stopPrice argument"))"###);
        };
        let mut query: Value = extend_2(params.clone(), Value::Json(normalize(&Value::Json(json!({
            "stopPrice": stop_price
        }))).unwrap()));
        return self.create_order(symbol.clone(), r#type.clone(), side.clone(), amount.clone(), price.clone(), query.clone()).await;
    }

    async fn create_stop_limit_order(&mut self, mut symbol: Value, mut side: Value, mut amount: Value, mut price: Value, mut stop_price: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        if !self.get("has".into()).get(Value::from("createStopLimitOrder")).is_truthy() {
            panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" createStopLimitOrder() is not supported yet"))"###);
        };
        let mut query: Value = extend_2(params.clone(), Value::Json(normalize(&Value::Json(json!({
            "stopPrice": stop_price
        }))).unwrap()));
        return self.create_order(symbol.clone(), Value::from("limit"), side.clone(), amount.clone(), price.clone(), query.clone()).await;
    }

    async fn create_stop_market_order(&mut self, mut symbol: Value, mut side: Value, mut amount: Value, mut stop_price: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        if !self.get("has".into()).get(Value::from("createStopMarketOrder")).is_truthy() {
            panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" createStopMarketOrder() is not supported yet"))"###);
        };
        let mut query: Value = extend_2(params.clone(), Value::Json(normalize(&Value::Json(json!({
            "stopPrice": stop_price
        }))).unwrap()));
        return self.create_order(symbol.clone(), Value::from("market"), side.clone(), amount.clone(), Value::Undefined, query.clone()).await;
    }

    fn safe_currency_code(&self, mut currency_id: Value, mut currency: Value) -> Value {
        currency = self.safe_currency(currency_id.clone(), currency.clone());
        return currency.get(Value::from("code"));
    }

    fn filter_by_symbol_since_limit(&self, mut array: Value, mut symbol: Value, mut since: Value, mut limit: Value, mut tail: Value) -> Value {
        tail = tail.or_default(false.into());
        return self.filter_by_value_since_limit(array.clone(), Value::from("symbol"), symbol.clone(), since.clone(), limit.clone(), Value::from("timestamp"), tail.clone());
    }

    fn filter_by_currency_since_limit(&self, mut array: Value, mut code: Value, mut since: Value, mut limit: Value, mut tail: Value) -> Value {
        tail = tail.or_default(false.into());
        return self.filter_by_value_since_limit(array.clone(), Value::from("currency"), code.clone(), since.clone(), limit.clone(), Value::from("timestamp"), tail.clone());
    }

    fn parse_tickers(&self, mut tickers: Value, mut symbols: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        //
        // the value of tickers is either a dict or a list
        //
        // dict
        //
        //     {
        //         'marketId1': { ... },
        //         'marketId2': { ... },
        //         'marketId3': { ... },
        //         ...
        //     }
        //
        // list
        //
        //     [
        //         { 'market': 'marketId1', ... },
        //         { 'market': 'marketId2', ... },
        //         { 'market': 'marketId3', ... },
        //         ...
        //     ]
        //
        let mut results: Value = Value::new_array();
        if Array::is_array(tickers.clone()).is_truthy() {
            let mut i: usize = 0;
            while i < tickers.len() {
                let mut ticker: Value = extend_2(self.parse_ticker(tickers.get(i.into()), Value::Undefined), params.clone());
                results.push(ticker.clone());
                i += 1;
            };
        } else {
            let mut market_ids: Value = Object::keys(tickers.clone());
            let mut i: usize = 0;
            while i < market_ids.len() {
                let mut market_id: Value = market_ids.get(i.into());
                let mut market: Value = self.safe_market(market_id.clone(), Value::Undefined, Value::Undefined);
                let mut ticker: Value = extend_2(self.parse_ticker(tickers.get(market_id.clone()), market.clone()), params.clone());
                results.push(ticker.clone());
                i += 1;
            };
        };
        symbols = self.market_symbols(symbols.clone());
        return self.filter_by_array(results.clone(), Value::from("symbol"), symbols.clone(), Value::Undefined);
    }

    fn parse_deposit_addresses(&self, mut addresses: Value, mut codes: Value, mut indexed: Value, mut params: Value) -> Value {
        indexed = indexed.or_default(true.into());
        params = params.or_default(Value::new_object());
        let mut result: Value = Value::new_array();
        let mut i: usize = 0;
        while i < addresses.len() {
            let mut address: Value = extend_2(self.parse_deposit_address(addresses.get(i.into()), Value::Undefined), params.clone());
            result.push(address.clone());
            i += 1;
        };
        if codes.clone().is_nonnullish() {
            result = self.filter_by_array(result.clone(), Value::from("currency"), codes.clone(), false.into());
        };
        result = if indexed.is_truthy() { self.index_by(result.clone(), Value::from("currency"), Value::Undefined) } else { result.clone() };
        return result.clone();
    }

    fn parse_borrow_interests(&self, mut response: Value, mut market: Value) -> Value {
        let mut interests: Value = Value::new_array();
        let mut i: usize = 0;
        while i < response.len() {
            let mut row: Value = response.get(i.into());
            interests.push(self.parse_borrow_interest(row.clone(), market.clone()));
            i += 1;
        };
        return interests.clone();
    }

    fn parse_funding_rate_histories(&self, mut response: Value, mut market: Value, mut since: Value, mut limit: Value) -> Value {
        let mut rates: Value = Value::new_array();
        let mut i: usize = 0;
        while i < response.len() {
            let mut entry: Value = response.get(i.into());
            rates.push(self.parse_funding_rate_history(entry.clone(), market.clone()));
            i += 1;
        };
        let mut sorted: Value = self.sort_by(rates.clone(), Value::from("timestamp"), Value::Undefined, Value::Undefined);
        let mut symbol: Value = if market.clone().is_nullish() { Value::Undefined } else { market.get(Value::from("symbol")) };
        return self.filter_by_symbol_since_limit(sorted.clone(), symbol.clone(), since.clone(), limit.clone(), Value::Undefined);
    }

    fn safe_symbol(&self, mut market_id: Value, mut market: Value, mut delimiter: Value) -> Value {
        market = self.safe_market(market_id.clone(), market.clone(), delimiter.clone());
        return market.get(Value::from("symbol"));
    }

    fn parse_funding_rate(&self, mut contract: Value, mut market: Value) -> Value {
        panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" parseFundingRate() is not supported yet"))"###);
        Value::Undefined
    }

    fn parse_funding_rates(&self, mut response: Value, mut market: Value) -> Value {
        let mut result: Value = Value::new_object();
        let mut i: usize = 0;
        while i < response.len() {
            let mut parsed: Value = self.parse_funding_rate(response.get(i.into()), market.clone());
            result.set(parsed.get(Value::from("symbol")), parsed.clone());
            i += 1;
        };
        return result.clone();
    }

    /// Returns true if a post only order, false otherwise
    ///
    /// @ignore
    ///
    /// # Arguments
    ///
    /// * `type` {string} - Order type
    /// * `exchangeSpecificParam` {boolean} - exchange specific postOnly
    /// * `params` {object} - exchange specific params
    fn is_post_only(&mut self, mut is_market_order: Value, mut exchange_specific_param: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        let mut time_in_force: Value = self.safe_string_upper(params.clone(), Value::from("timeInForce"), Value::Undefined);
        let mut post_only: Value = self.safe_value_2(params.clone(), Value::from("postOnly"), Value::from("post_only"), false.into());
        // we assume timeInForce is uppercase from safeStringUpper (params, 'timeInForce')
        let mut ioc: Value = (time_in_force.clone() == Value::from("IOC")).into();
        let mut fok: Value = (time_in_force.clone() == Value::from("FOK")).into();
        let mut time_in_force_post_only: Value = (time_in_force.clone() == Value::from("PO")).into();
        post_only = (post_only.is_truthy() || time_in_force_post_only.is_truthy() || exchange_specific_param.is_truthy()).into();
        if post_only.is_truthy() {
            if ioc.is_truthy() || fok.is_truthy() {
                panic!(r###"InvalidOrder::new(self.get("id".into()) + Value::from(" postOnly orders cannot have timeInForce equal to ") + time_in_force.clone())"###);
            } else if is_market_order.is_truthy() {
                panic!(r###"InvalidOrder::new(self.get("id".into()) + Value::from(" market orders cannot be postOnly"))"###);
            } else {
                return true.into();
            };
        } else {
            return false.into();
        };
        Value::Undefined
    }

    async fn fetch_trading_fees(&mut self, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchTradingFees() is not supported yet"))"###);
        Value::Undefined
    }

    async fn fetch_trading_fee(&mut self, mut symbol: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        if !self.get("has".into()).get(Value::from("fetchTradingFees")).is_truthy() {
            panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchTradingFee() is not supported yet"))"###);
        };
        return self.fetch_trading_fees(params.clone()).await;
    }

    fn parse_open_interest(&self, mut interest: Value, mut market: Value) -> Value {
        panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" parseOpenInterest () is not supported yet"))"###);
        Value::Undefined
    }

    fn parse_open_interests(&self, mut response: Value, mut market: Value, mut since: Value, mut limit: Value) -> Value {
        let mut interests: Value = Value::new_array();
        let mut i: usize = 0;
        while i < response.len() {
            let mut entry: Value = response.get(i.into());
            let mut interest: Value = self.parse_open_interest(entry.clone(), market.clone());
            interests.push(interest.clone());
            i += 1;
        };
        let mut sorted: Value = self.sort_by(interests.clone(), Value::from("timestamp"), Value::Undefined, Value::Undefined);
        let mut symbol: Value = self.safe_string(market.clone(), Value::from("symbol"), Value::Undefined);
        return self.filter_by_symbol_since_limit(sorted.clone(), symbol.clone(), since.clone(), limit.clone(), Value::Undefined);
    }

    async fn fetch_funding_rate(&mut self, mut symbol: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        if self.get("has".into()).get(Value::from("fetchFundingRates")).is_truthy() {
            self.load_markets(Value::Undefined, Value::Undefined).await;
            let mut market: Value = self.market(symbol.clone());
            if !market.get(Value::from("contract")).is_truthy() {
                panic!(r###"BadSymbol::new(self.get("id".into()) + Value::from(" fetchFundingRate() supports contract markets only"))"###);
            };
            let mut rates: Value = self.fetch_funding_rates(Value::Json(serde_json::Value::Array(vec![symbol.clone().into()])), params.clone()).await;
            let mut rate: Value = self.safe_value(rates.clone(), symbol.clone(), Value::Undefined);
            if rate.clone().is_nullish() {
                panic!(r###"NullResponse::new(self.get("id".into()) + Value::from(" fetchFundingRate () returned no data for ") + symbol.clone())"###);
            } else {
                return rate.clone();
            };
        } else {
            panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchFundingRate () is not supported yet"))"###);
        };
        Value::Undefined
    }

    /// Returns a list of candles ordered as timestamp, open, high, low, close, undefined
    ///
    /// Fetches historical mark price candlestick data containing the open, high, low, and close price of a market
    ///
    /// # Arguments
    ///
    /// * `symbol` {string} - unified symbol of the market to fetch OHLCV data for
    /// * `timeframe` {string} - the length of time each candle represents
    /// * `since` {int|undefined} - timestamp in ms of the earliest candle to fetch
    /// * `limit` {int|undefined} - the maximum amount of candles to fetch
    /// * `params` {object} - extra parameters specific to the exchange api endpoint
    async fn fetch_mark_ohlcv(&mut self, mut symbol: Value, mut timeframe: Value, mut since: Value, mut limit: Value, mut params: Value) -> Value {
        timeframe = timeframe.or_default(Value::from("1m"));
        params = params.or_default(Value::new_object());
        if self.get("has".into()).get(Value::from("fetchMarkOHLCV")).is_truthy() {
            let mut request: Value = Value::Json(normalize(&Value::Json(json!({
                "price": "mark"
            }))).unwrap());
            return self.fetch_ohlcv(symbol.clone(), timeframe.clone(), since.clone(), limit.clone(), extend_2(request.clone(), params.clone())).await;
        } else {
            panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchMarkOHLCV () is not supported yet"))"###);
        };
        Value::Undefined
    }

    /// Returns a list of candles ordered as timestamp, open, high, low, close, undefined
    ///
    /// Fetches historical index price candlestick data containing the open, high, low, and close price of a market
    ///
    /// # Arguments
    ///
    /// * `symbol` {string} - unified symbol of the market to fetch OHLCV data for
    /// * `timeframe` {string} - the length of time each candle represents
    /// * `since` {int|undefined} - timestamp in ms of the earliest candle to fetch
    /// * `limit` {int|undefined} - the maximum amount of candles to fetch
    /// * `params` {object} - extra parameters specific to the exchange api endpoint
    async fn fetch_index_ohlcv(&mut self, mut symbol: Value, mut timeframe: Value, mut since: Value, mut limit: Value, mut params: Value) -> Value {
        timeframe = timeframe.or_default(Value::from("1m"));
        params = params.or_default(Value::new_object());
        if self.get("has".into()).get(Value::from("fetchIndexOHLCV")).is_truthy() {
            let mut request: Value = Value::Json(normalize(&Value::Json(json!({
                "price": "index"
            }))).unwrap());
            return self.fetch_ohlcv(symbol.clone(), timeframe.clone(), since.clone(), limit.clone(), extend_2(request.clone(), params.clone())).await;
        } else {
            panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchIndexOHLCV () is not supported yet"))"###);
        };
        Value::Undefined
    }

    /// Returns a list of candles ordered as timestamp, open, high, low, close, undefined
    ///
    /// Fetches historical premium index price candlestick data containing the open, high, low, and close price of a market
    ///
    /// # Arguments
    ///
    /// * `symbol` {string} - unified symbol of the market to fetch OHLCV data for
    /// * `timeframe` {string} - the length of time each candle represents
    /// * `since` {int|undefined} - timestamp in ms of the earliest candle to fetch
    /// * `limit` {int|undefined} - the maximum amount of candles to fetch
    /// * `params` {object} - extra parameters specific to the exchange api endpoint
    async fn fetch_premium_index_ohlcv(&mut self, mut symbol: Value, mut timeframe: Value, mut since: Value, mut limit: Value, mut params: Value) -> Value {
        timeframe = timeframe.or_default(Value::from("1m"));
        params = params.or_default(Value::new_object());
        if self.get("has".into()).get(Value::from("fetchPremiumIndexOHLCV")).is_truthy() {
            let mut request: Value = Value::Json(normalize(&Value::Json(json!({
                "price": "premiumIndex"
            }))).unwrap());
            return self.fetch_ohlcv(symbol.clone(), timeframe.clone(), since.clone(), limit.clone(), extend_2(request.clone(), params.clone())).await;
        } else {
            panic!(r###"NotSupported::new(self.get("id".into()) + Value::from(" fetchPremiumIndexOHLCV () is not supported yet"))"###);
        };
        Value::Undefined
    }

    /// Returns returns the exchange specific value for timeInForce
    ///
    /// @ignore
    /// * Must add timeInForce to this.options to use this method
    fn handle_time_in_force(&mut self, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        let mut time_in_force: Value = self.safe_string_upper(params.clone(), Value::from("timeInForce"), Value::Undefined);
        // supported values GTC, IOC, PO
        if time_in_force.clone().is_nonnullish() {
            let mut exchange_value: Value = self.safe_string(self.get("options".into()).get(Value::from("timeInForce")), time_in_force.clone(), Value::Undefined);
            if exchange_value.clone().is_nullish() {
                panic!(r###"ExchangeError::new(self.get("id".into()) + Value::from(r#" does not support timeInForce ""#) + time_in_force.clone() + Value::from(r#"""#))"###);
            };
            return exchange_value.clone();
        };
        return Value::Undefined;
    }

    /// Returns the exchange specific account name or the isolated margin id for transfers
    ///
    /// @ignore
    /// * Must add accountsByType to this.options to use this method
    ///
    /// # Arguments
    ///
    /// * `account` {string} - key for account name in this.options['accountsByType']
    fn parse_account(&self, mut account: Value) -> Value {
        let mut accounts_by_type: Value = self.safe_value(self.get("options".into()), Value::from("accountsByType"), Value::new_object());
        let mut symbols: Value = self.get("symbols".into());
        if accounts_by_type.contains_key(account.clone()) {
            return accounts_by_type.get(account.clone());
        } else if self.in_array(account.clone(), symbols.clone()).is_truthy() {
            let mut market: Value = self.market(account.clone());
            return market.get(Value::from("id"));
        } else {
            return account.clone();
        };
        Value::Undefined
    }

    /// Returns {[string|undefined, object]} the marginMode in lowercase as specified by params["marginMode"], params["defaultMarginMode"] this.options["marginMode"] or this.options["defaultMarginMode"]
    ///
    /// @ignore
    ///
    /// # Arguments
    ///
    /// * `params` {object} - extra parameters specific to the exchange api endpoint
    fn handle_margin_mode_and_params(&mut self, mut method_name: Value, mut params: Value) -> Value {
        params = params.or_default(Value::new_object());
        let mut default_margin_mode: Value = self.safe_string_2(self.get("options".into()), Value::from("marginMode"), Value::from("defaultMarginMode"), Value::Undefined);
        let mut method_options: Value = self.safe_value(self.get("options".into()), method_name.clone(), Value::new_object());
        let mut method_margin_mode: Value = self.safe_string_2(method_options.clone(), Value::from("marginMode"), Value::from("defaultMarginMode"), default_margin_mode.clone());
        let mut margin_mode: Value = self.safe_string_lower_2(params.clone(), Value::from("marginMode"), Value::from("defaultMarginMode"), method_margin_mode.clone());
        if margin_mode.clone().is_nonnullish() {
            params = self.omit(params.clone(), Value::Json(serde_json::Value::Array(vec![Value::from("marginMode").into(), Value::from("defaultMarginMode").into()])));
        };
        return Value::Json(serde_json::Value::Array(vec![margin_mode.clone().into(), params.clone().into()]));
    }

    async fn load_markets_helper(&mut self, mut reload: Value, mut params: Value) -> Value {
        reload = reload.or_default(false.into());
        params = params.or_default(Value::new_object());
        if !reload.is_truthy() && self.get("markets".into()).is_truthy() {
            if !self.get("markets_by_id".into()).is_truthy() {
                return self.set_markets(self.get("markets".into()), Value::Undefined);
            };
            return self.get("markets".into());
        };
        let mut currencies: Value = Value::Undefined;
        // only call if exchange API provides endpoint (true), thus avoid emulated versions ('emulated')
        if self.get("has".into()).get(Value::from("fetchCurrencies")) == true.into() {
            currencies = self.fetch_currencies(Value::Undefined).await;
        };
        let mut markets: Value = self.fetch_markets(params.clone()).await;
        return self.set_markets(markets.clone(), currencies.clone());
    }

    async fn load_markets(&mut self, mut reload: Value, mut params: Value) -> Value {
        reload = reload.or_default(false.into());
        params = params.or_default(Value::new_object());
        // this method is async, it returns a promise
        if reload.is_truthy() && !self.get("reloading_markets".into()).is_truthy() || !self.get("markets_loading".into()).is_truthy() {
            self.set("reloading_markets".into(), true.into());
            // TODO This should use a finally block
            let mut markets_loading: Value = self.load_markets_helper(reload.clone(), params.clone()).await;
            self.set("markets_loading".into(), markets_loading.clone());
            self.set("reloading_markets".into(), false.into());
            return self.get("markets_loading".into());
        };
        return self.get("markets_loading".into());
    }

}