use crate::mesh::material::Material;
use crate::render::intersection::{Intersectable, Intersection};
use crate::render::ray::Ray;
use crate::Vec3;

pub struct Plane {
    pub position: Vec3,
    pub normal: Vec3,
    pub material: Material,
}

impl Intersectable for Plane {
    fn intersect(&self, ray: &Ray) -> Option<Intersection> {
        if f32::abs(ray.direction.y) > 0.001 {
            let d = -(ray.origin.y + 4.) / ray.direction.y;
            let p = ray.origin + ray.direction * d;
            if d > 0.001 && f32::abs(p.x) < 10. && p.z < -10. && p.z > -30. {
                let point_intersect = p;
                let normal = Vec3::new(0., 1., 0.);
                let mut material = Material::new_default();
                material.diffuse_color = if (((0.5 * point_intersect.x + 1000.) as i32
                    + (0.5 * point_intersect.z) as i32)
                    & 1)
                    == 0
                {
                    Vec3::new(0.3, 0.3, 0.3)
                } else {
                    Vec3::new(0.3, 0.2, 0.1)
                };
                return Some(Intersection::new(d, point_intersect, normal, material));
            }
        }
        None
    }
}

impl Plane {
    pub const fn new(position: Vec3, normal: Vec3, material: Material) -> Plane {
        Plane {
            position,
            normal,
            material,
        }
    }
}
