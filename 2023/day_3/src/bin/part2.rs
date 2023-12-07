mod part1;

use std::collections::HashMap;
use std::env;
use std::fs;

use part1::{Coord, Manual, ManualNumberCoords};

type Symbol = (String, Coord);

struct ManualNumberValueAndCoords {
    value: i32,
    value_coords: Vec<Coord>,
    surrounding_symbols: Vec<Symbol>,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_contents = part1::get_input(&args[1]);

    let matrix = part1::format_to_vec(&file_contents);

    let all_number_coords = part1::get_number_coords(&matrix);

    let all_numbers: Vec<ManualNumberValueAndCoords> = all_number_coords
        .iter()
        .map(|num| manual_number_coords_with_values(&matrix, num))
        .collect();

    let star_adjacent_numbers: Vec<&ManualNumberValueAndCoords> =
        all_numbers.iter().filter(|num| is_star_num(&num)).collect();

    let map_of_star_nums = build_hash_map(&star_adjacent_numbers);

    let total: i32 = map_of_star_nums
        .iter()
        .filter(|(_, value)| value.len() == 2)
        .map(|(_, value)| value[0] * value[1])
        .sum();

    println!("{}", total);
}

fn manual_number_coords_with_values(
    matrix: &Manual,
    num: &ManualNumberCoords,
) -> ManualNumberValueAndCoords {
    let value = part1::coords_to_num(matrix, &num.value);
    let symbols: Vec<Symbol> = num
        .surrounding_symbols
        .iter()
        .map(|sym| coord_to_symbol(matrix, *sym))
        .collect();

    ManualNumberValueAndCoords {
        value,
        value_coords: num.value.to_owned(),
        surrounding_symbols: symbols,
    }
}

fn build_hash_map(nums: &Vec<&ManualNumberValueAndCoords>) -> HashMap<Coord, Vec<i32>> {
    let mut coord_map: HashMap<Coord, Vec<i32>> = HashMap::new();

    for num in nums {
        let star_coords: Vec<Coord> = num
            .surrounding_symbols
            .iter()
            .filter(|(sym, _)| is_star(sym))
            .map(|(_, coord)| *coord)
            .collect();

        for coord in star_coords {
            let nums_for_coord = coord_map.get_mut(&coord);

            match nums_for_coord {
                Some(vector) => {
                    vector.push(num.value);
                }
                None => {
                    coord_map.insert(coord, vec![num.value]);
                }
            }
        }
    }

    coord_map
}

fn coord_to_symbol(matrix: &Manual, (x, y): Coord) -> Symbol {
    let symbol = matrix[y][x].to_owned();
    (symbol, (x, y))
}

fn is_star_num(num: &ManualNumberValueAndCoords) -> bool {
    num.surrounding_symbols.iter().any(|(sym, _)| is_star(sym))
}

fn is_star(string: &String) -> bool {
    string == "*"
}
