use image::Rgb;

pub struct Camera {
    pub width: u32,
    pub height: u32,
    pub fov: f32,
    pub background_color: Rgb<u8>,
}

impl Camera {
    pub const fn new(width: u32, height: u32, fov: f32, background_color: Rgb<u8>) -> Camera {
        Camera {
            width,
            height,
            fov,
            background_color,
        }
    }
}
