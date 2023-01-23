use crate::mesh::material::Material;
use crate::render::intersection::{Intersectable, Intersection};
use crate::render::ray::Ray;
use crate::Vec3;

#[derive(Debug, Copy, Clone)]
pub struct Sphere {
    pub center: Vec3,
    pub radius: f32,
    pub material: Material,
}

impl Intersectable for Sphere {
    fn intersect(&self, ray: &Ray) -> Option<Intersection> {
        let (intersection, d) = self.ray_intersect(ray);
        if !intersection {
            return None;
        }
        let point_intersect = ray.origin + ray.direction * d;
        let normal = (point_intersect - self.center).normalized();
        let material = self.material;
        Some(Intersection::new(d, point_intersect, normal, material))
    }
}

impl Sphere {
    pub const fn new(center: Vec3, radius: f32, material: Material) -> Sphere {
        Self {
            center,
            radius,
            material,
        }
    }

    fn ray_intersect(&self, from: &Ray) -> (bool, f32) {
        let l = self.center - from.origin;
        let tca = l * from.direction;
        let d2 = l * l - tca * tca;
        if d2 > self.radius * self.radius {
            return (false, 0.);
        }
        let thc = f32::sqrt(self.radius * self.radius - d2);
        let t0 = tca - thc;
        let t1 = tca + thc;
        if t0 > 0.001 {
            return (true, t0);
        }
        if t1 > 0.001 {
            return (true, t1);
        }
        (false, 0.)
    }
}
