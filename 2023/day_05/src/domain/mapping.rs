use super::mapping_range::{parse_range, MappingRange};

#[derive(Debug)]
pub struct Mapping {
    mapping_ranges: Vec<MappingRange>,
}

impl Mapping {
    pub fn map(&self, num: i64) -> i64 {
        for range in &self.mapping_ranges {
            if range.is_in_range(num) {
                return range.map(num);
            }
        }
        num
    }
}

pub fn parse_mapping(lines: &[String]) -> Mapping {
    let range_lines = &lines[1..];

    let ranges: Vec<MappingRange> = range_lines.iter().map(|l| parse_range(l)).collect();

    Mapping {
        mapping_ranges: ranges,
    }
}
