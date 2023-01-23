use crate::mesh::element::Element;
use crate::mesh::light::Light;
use crate::mesh::material::Material;
use crate::mesh::plane::Plane;
use crate::render::camera::Camera;
use crate::render::intersection::{Intersectable, Intersection};
use crate::render::ray::Ray;
use crate::render::scene::Scene;
use image::Rgb;
use mesh::sphere::Sphere;
use std::path::Path;
use vector::vec3::Vec3;

mod mesh;
mod render;
mod vector;

const WIDTH: u32 = 1024;
const HEIGHT: u32 = 728;
const FOV: f32 = std::f32::consts::PI / 3.;

const IVORY: Material = Material::new(1., [0.9, 0.5, 0.1, 0.0], Vec3::new(0.4, 0.4, 0.3), 50.);
const GLASS: Material = Material::new(1.5, [0.0, 0.9, 0.1, 0.8], Vec3::new(0.6, 0.7, 0.8), 125.);
const RED_RUBBER: Material =
    Material::new(1.0, [1.4, 0.3, 0.0, 0.0], Vec3::new(0.3, 0.1, 0.1), 10.);
const MIRROR: Material = Material::new(1.0, [0.0, 16.0, 0.8, 0.0], Vec3::new(1.0, 1.0, 1.0), 1425.);

fn main() {
    let mut scene = Scene::new(Camera::new(WIDTH, HEIGHT, FOV, Rgb([51, 180, 205])));
    scene.add_element(Element::Plane(Plane::new(
        Vec3::zero(),
        Vec3::zero(),
        Material::new_default(),
    )));
    scene.add_element(Element::Sphere(Sphere::new(
        Vec3::new(-3., 0., -16.),
        1.,
        IVORY,
    )));
    scene.add_element(Element::Sphere(Sphere::new(
        Vec3::new(-1.0, -1.5, -12.),
        2.,
        GLASS,
    )));
    scene.add_element(Element::Sphere(Sphere::new(
        Vec3::new(1.5, -0.5, -18.),
        3.,
        RED_RUBBER,
    )));
    scene.add_element(Element::Sphere(Sphere::new(
        Vec3::new(7., 5., -18.),
        4.,
        MIRROR,
    )));
    scene.add_light(Light::new(Vec3::new(-20., 20., 20.)));
    scene.add_light(Light::new(Vec3::new(30., 50., -25.)));
    scene.add_light(Light::new(Vec3::new(30., 20., 30.)));
    let img = scene.render();
    let res = image::save_buffer(
        &Path::new("res.png"),
        img.as_raw(),
        img.width(),
        img.height(),
        image::ColorType::Rgb8,
    );
    println!("{:?}", res);
}
