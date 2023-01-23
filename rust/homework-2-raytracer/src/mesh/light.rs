use crate::Vec3;

pub struct Light {
    pub position: Vec3,
}

impl Light {
    pub fn new(position: Vec3) -> Light {
        Light { position }
    }
}
