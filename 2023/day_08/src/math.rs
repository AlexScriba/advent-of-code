
type Num = u128;

pub fn gcd(a: Num, b: Num) -> Num {
    if b == 0 {
        return a;
    }

    gcd(b, a % b)
}

pub fn lcm(a: Num, b: Num) -> Num {
    (a*b) / gcd(a, b)
}

pub fn lcm_vec(nums: &[Num]) -> Num {
    let mut ans = nums[0];

    for num in nums[1..].iter() {
        ans = lcm(ans, *num);
    }

    ans
}

#[cfg(test)]
mod math_tests {
    use super::*;

    #[test]
    fn gcd_works() {
        assert_eq!(3, gcd(9, 21));
    }

    #[test]
    fn lcm_works() {
        assert_eq!(21, lcm(3, 7));
    }

    #[test]
    fn lcm_vec_works() {
        assert_eq!(84, lcm_vec(&[3, 7, 12]));
    }
}
