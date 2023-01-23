use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Debug, Copy, Clone)]
pub struct Vec3 {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

impl Vec3 {
    pub const fn zero() -> Vec3 {
        Self {
            x: 0.,
            y: 0.,
            z: 0.,
        }
    }

    pub const fn new(x: f32, y: f32, z: f32) -> Vec3 {
        Self { x, y, z }
    }

    pub fn length_squared(self) -> f32 {
        self.x * self.x + self.y * self.y + self.z * self.z
    }

    pub fn length(&self) -> f32 {
        f32::sqrt(self.length_squared())
    }

    pub fn normalized(&self) -> Vec3 {
        *self / self.length()
    }
}

impl Add for Vec3 {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
}

impl Add<f32> for Vec3 {
    type Output = Self;

    fn add(self, other: f32) -> Self {
        Self {
            x: self.x + other,
            y: self.y + other,
            z: self.z + other,
        }
    }
}

impl Sub for Vec3 {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
            x: self.x - other.x,
            y: self.y - other.y,
            z: self.z - other.z,
        }
    }
}

impl Sub<f32> for Vec3 {
    type Output = Self;

    fn sub(self, other: f32) -> Self {
        Self {
            x: self.x - other,
            y: self.y - other,
            z: self.z - other,
        }
    }
}

impl Neg for Vec3 {
    type Output = Self;

    fn neg(self) -> Self {
        Self {
            x: -self.x,
            y: -self.y,
            z: -self.z,
        }
    }
}

impl Mul for Vec3 {
    type Output = f32;

    fn mul(self, other: Self) -> f32 {
        self.x * other.x + self.y * other.y + self.z * other.z
    }
}

impl Mul<f32> for Vec3 {
    type Output = Self;

    fn mul(self, other: f32) -> Self {
        Self {
            x: self.x * other,
            y: self.y * other,
            z: self.z * other,
        }
    }
}

impl Div<f32> for Vec3 {
    type Output = Self;

    fn div(self, other: f32) -> Self {
        Self {
            x: self.x / other,
            y: self.y / other,
            z: self.z / other,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::vector::vec3::Vec3;

    const EPS: f32 = 1e-1;

    fn equals_f32(a: f32, b: f32) -> bool {
        let abs_a = f32::abs(a);
        let abs_b = f32::abs(b);
        let diff = f32::abs(a - b);

        if a == b {
            true
        } else if a == 0. || b == 0. || diff < f32::MIN_POSITIVE {
            diff < (EPS * f32::MIN_POSITIVE)
        } else {
            diff / (abs_a + abs_b) < EPS
        }
    }

    fn equals_vec3(a: Vec3, b: Vec3) -> bool {
        equals_f32(a.x, b.x) && equals_f32(a.y, b.y) && equals_f32(a.z, b.z)
    }

    fn assert_eq_f32(a: f32, b: f32) {
        assert!(equals_f32(a, b));
    }

    fn assert_eq_vec3(a: Vec3, b: Vec3) {
        assert!(equals_vec3(a, b))
    }

    #[test]
    fn test_vec3_add_vec3() {
        let vec1 = Vec3::new(1.0, 5.0, 7.0);
        let vec2 = Vec3::new(-5.0, 10.0, 0.0);
        assert_eq_vec3(Vec3::new(-4.0, 15.0, 7.0), vec1 + vec2);
    }

    #[test]
    fn test_vec3_add_f32() {
        let vec1 = Vec3::new(1.0, 5.0, 7.0);
        assert_eq_vec3(Vec3::new(4.0, 8.0, 10.0), vec1 + 3.);
    }

    #[test]
    fn test_vec3_sub_vec3() {
        let vec1 = Vec3::new(1.0, 5.0, 7.0);
        let vec2 = Vec3::new(-5.0, 10.0, 2.0);
        assert_eq_vec3(Vec3::new(6.0, -5.0, 5.0), vec1 - vec2);
    }

    #[test]
    fn test_vec3_sub_f32() {
        let vec1 = Vec3::new(1.0, 5.0, 7.0);
        assert_eq_vec3(Vec3::new(-2.0, 2.0, 4.0), vec1 - 3.);
    }

    #[test]
    fn test_neg_vec3() {
        let vec1 = Vec3::new(1.0, 5.0, 7.0);
        assert_eq_vec3(Vec3::new(-1.0, -5.0, -7.0), -vec1);
    }

    #[test]
    fn test_vec3_mul_vec3() {
        let vec1 = Vec3::new(1.0, 5.0, 7.0);
        let vec2 = Vec3::new(-5.0, 10.0, 1.0);
        assert_eq_f32(-5. * 1. + 5. * 10. + 7., vec1 * vec2);
    }

    #[test]
    fn test_vec3_mul_f32() {
        let vec1 = Vec3::new(1.0, 5.0, 7.0);
        assert_eq_vec3(Vec3::new(3.0, 15.0, 21.0), vec1 * 3.);
    }

    #[test]
    fn test_vec3_div_f32() {
        let vec1 = Vec3::new(20.0, 50.0, 100.0);
        assert_eq_vec3(Vec3::new(10., 25., 50.), vec1 / 2.);
    }
}
