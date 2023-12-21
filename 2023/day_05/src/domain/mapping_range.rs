#[derive(Debug)]
pub struct MappingRange {
    pub source_start: i64,
    pub destination_start: i64,
    pub length: i64,
}

impl MappingRange {
    pub fn is_in_range(&self, num: i64) -> bool {
        num >= self.source_start && num < (self.source_start + self.length)
    }

    pub fn map(&self, num: i64) -> i64 {
        num + self.offset()
    }

    pub fn offset(&self) -> i64 {
        self.destination_start - self.source_start
    }
}

pub fn parse_range(string: &String) -> MappingRange {
    let numbers: Vec<i64> = string
        .split(" ")
        .map(|n| {
            n.parse::<i64>()
                .expect(&format!("Could not parse number [{}]", n))
        })
        .collect();

    MappingRange {
        source_start: numbers[1],
        destination_start: numbers[0],
        length: numbers[2],
    }
}
