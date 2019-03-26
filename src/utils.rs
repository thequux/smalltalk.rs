use std::ops::{Rem, Div, Neg, Sub, Mul, Add};
use std::fmt::Debug;

pub fn floor_divmod<D>(mut a: D, mut b: D) -> (D, D)
    where
        D: Div<Output=D>,
        D: Mul<Output=D>,
        D: Rem<Output=D>,
        D: Neg<Output=D>,
        D: Sub<Output=D>,
        D: Add<Output=D>,
        D: Copy + PartialEq<D> + Debug,
        D: PartialOrd<D>, {
    let zero = a - a;

    if b == zero {
        panic!("Divide by zero");
    }

    let b_abs = if b < zero { -b } else { b };
    let mut v_mod = a % b;
    if v_mod < zero {
            v_mod = v_mod + b_abs;
    }
    assert!(v_mod >= zero);
    assert!(v_mod < b_abs);

    let v_div = (a-v_mod)/b;
    assert_eq!(v_div * b + v_mod, a);

    (v_div, v_mod)
}