use crate::Vec3;

#[derive(Debug, Copy, Clone)]
pub struct Material {
    pub refractive_index: f32,
    pub albedo: [f32; 4],
    pub diffuse_color: Vec3,
    pub specular_exponent: f32,
}

impl Material {
    pub const fn new(
        refractive_index: f32,
        albedo: [f32; 4],
        diffuse_color: Vec3,
        specular_exponent: f32,
    ) -> Material {
        Self {
            refractive_index,
            albedo,
            diffuse_color,
            specular_exponent,
        }
    }

    pub const fn new_default() -> Material {
        Self::new(1., [2., 0., 0., 0.], Vec3::zero(), 0.)
    }
}
