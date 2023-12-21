use super::mapping::Mapping;

#[derive(Debug)]
pub struct MappingPipeline {
    pub layers: Vec<Mapping>,
}

impl MappingPipeline {
    pub fn map_through(&self, num: i64) -> i64 {
        let mut res_num = num;

        for layer in &self.layers {
            res_num = layer.map(res_num);
        }

        res_num
    }
}
