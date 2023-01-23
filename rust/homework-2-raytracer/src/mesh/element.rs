use crate::render::intersection::Intersection;
use crate::{Intersectable, Plane, Ray, Sphere};

pub enum Element {
    Sphere(Sphere),
    Plane(Plane),
}

impl Intersectable for Element {
    fn intersect(&self, ray: &Ray) -> Option<Intersection> {
        match self {
            Element::Sphere(s) => s.intersect(ray),
            Element::Plane(p) => p.intersect(ray),
        }
    }
}
