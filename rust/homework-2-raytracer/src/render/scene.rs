use crate::mesh::light::Light;
use crate::{Camera, Element, Intersectable, Intersection, Ray, Vec3};
use image::{ImageBuffer, Rgb, RgbImage};

pub struct Scene {
    camera: Camera,
    elements: Vec<Element>,
    lights: Vec<Light>,
    background_color: Vec3,
}

impl Scene {
    pub fn new(camera: Camera) -> Scene {
        let background_color = rgb_to_vec3(&camera.background_color);
        Scene {
            camera,
            elements: Vec::new(),
            lights: Vec::new(),
            background_color,
        }
    }

    pub fn add_element(&mut self, element: Element) {
        self.elements.push(element);
    }

    pub fn add_light(&mut self, light: Light) {
        self.lights.push(light);
    }

    pub fn render(&self) -> ImageBuffer<Rgb<u8>, Vec<u8>> {
        let width = self.camera.width;
        let height = self.camera.height;
        let mut img = RgbImage::new(width, height);
        for y in 0..height {
            for x in 0..width {
                let dir_x = (x as f32 + 0.5) - width as f32 / 2.;
                let dir_y = -(y as f32 + 0.5) + height as f32 / 2.;
                let dir_z = -(height as f32) / (2. * f32::tan(self.camera.fov / 2.));
                let ray = Ray::new(Vec3::new(0., 0., 0.), Vec3::new(dir_x, dir_y, dir_z));
                let pix = vec3_to_rgb(&self.cast_ray(ray, 0));
                img.put_pixel(x, y, pix);
            }
        }
        img
    }

    fn scene_intersect(&self, ray: &Ray) -> Option<Intersection> {
        self.elements
            .iter()
            .filter_map(|e| e.intersect(ray))
            .min_by(|i1, i2| i1.distance.partial_cmp(&i2.distance).unwrap())
    }

    fn reflect(direction: Vec3, normal: Vec3) -> Vec3 {
        direction - normal * 2. * (direction * normal)
    }

    fn refract(direction: Vec3, normal: Vec3, eta_t: f32, eta_i: f32) -> Vec3 {
        let cosi = -f32::max(-1., f32::min(1., direction * normal));
        if cosi < 0. {
            return Self::refract(direction, -normal, eta_i, eta_t);
        }
        let eta = eta_i / eta_t;
        let k = 1. - eta * eta * (1. - cosi * cosi);
        if k < 0. {
            Vec3::new(1., 0., 0.)
        } else {
            direction * eta + normal * (eta * cosi - f32::sqrt(k))
        }
    }

    fn cast_ray(&self, ray: Ray, depth: i32) -> Vec3 {
        if depth > 4 {
            return self.background_color;
        }
        let elements_intersect = self.scene_intersect(&ray);
        let elem_inter = if let Some(elem_inter) = elements_intersect {
            elem_inter
        } else {
            return self.background_color;
        };

        let reflect_dir = Self::reflect(ray.direction, elem_inter.normal).normalized();
        let refract_dir = Self::refract(
            ray.direction,
            elem_inter.normal,
            elem_inter.material.refractive_index,
            1.,
        )
        .normalized();
        let reflect_color =
            self.cast_ray(Ray::new(elem_inter.point_intersect, reflect_dir), depth + 1);
        let refract_color =
            self.cast_ray(Ray::new(elem_inter.point_intersect, refract_dir), depth + 1);
        let mut diffuse_light_intensity = 0.;
        let mut specular_light_intensity = 0.;
        for light in self.lights.iter() {
            let light_dir = (light.position - elem_inter.point_intersect).normalized();
            let light_intersection =
                self.scene_intersect(&Ray::new(elem_inter.point_intersect, light_dir));
            if let Some(inter) = light_intersection {
                if (inter.point_intersect - elem_inter.point_intersect).length()
                    < (light.position - elem_inter.point_intersect).length()
                {
                    continue;
                }
            }
            diffuse_light_intensity += f32::max(0., light_dir * elem_inter.normal);
            specular_light_intensity += f32::powf(
                f32::max(
                    0.,
                    ray.direction * (-Self::reflect(-light_dir, elem_inter.normal)),
                ),
                elem_inter.material.specular_exponent,
            );
        }
        elem_inter.material.diffuse_color * diffuse_light_intensity * elem_inter.material.albedo[0]
            + Vec3::new(1., 1., 1.) * specular_light_intensity * elem_inter.material.albedo[1]
            + reflect_color * elem_inter.material.albedo[2]
            + refract_color * elem_inter.material.albedo[3]
    }
}

fn vec3_to_rgb(vec: &Vec3) -> Rgb<u8> {
    Rgb([
        (vec.x * 256.) as u8,
        (vec.y * 256.) as u8,
        (vec.z * 256.) as u8,
    ])
}

fn rgb_to_vec3(color: &Rgb<u8>) -> Vec3 {
    Vec3::new(
        color[0] as f32 / 256.,
        color[1] as f32 / 256.,
        color[2] as f32 / 256.,
    )
}
