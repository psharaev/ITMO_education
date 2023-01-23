use crate::{Material, Ray, Vec3};

pub struct Intersection {
    pub distance: f32,
    pub point_intersect: Vec3,
    pub normal: Vec3,
    pub material: Material,
}

pub trait Intersectable {
    fn intersect(&self, ray: &Ray) -> Option<Intersection>;
}

impl Intersection {
    pub fn new(
        distance: f32,
        point_intersect: Vec3,
        normal: Vec3,
        material: Material,
    ) -> Intersection {
        if !distance.is_finite() {
            panic!("Intersection must have a finite distance.");
        }
        Intersection {
            distance,
            point_intersect,
            normal,
            material,
        }
    }
}
