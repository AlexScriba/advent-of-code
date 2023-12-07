use std::collections::HashSet;
use std::env;
use std::fs;

pub type Manual = Vec<Vec<String>>;
pub type Coord = (usize, usize);

pub struct ManualNumberCoords {
    pub value: Vec<Coord>,
    pub surrounding_symbols: Vec<Coord>,
}

pub struct ManualNumber {
    value: i32,
    surrounding_symbols: Vec<String>,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_contents = get_input(&args[1]);

    let matrix = format_to_vec(&file_contents);

    let all_numbers = get_numbers(&matrix);
    let part_numbers: Vec<&ManualNumber> = all_numbers
        .iter()
        .filter(|num| is_part_number(num))
        .collect();

    let total: i32 = part_numbers.iter().map(|num| num.value).sum();

    println!("{}", total);
}

pub fn get_numbers(matrix: &Manual) -> Vec<ManualNumber> {
    let number_coords = get_number_coords(matrix);
    number_coords_to_numbers(matrix, number_coords)
}

pub fn get_number_coords(matrix: &Manual) -> Vec<ManualNumberCoords> {
    let mut part_numbers: Vec<ManualNumberCoords> = vec![];
    let mut num_skips = 0;

    for (r, row) in matrix.iter().enumerate() {
        for (c, cell) in row.iter().enumerate() {
            if num_skips > 0 {
                num_skips -= 1;
                continue;
            }

            if is_number(&cell) {
                let number = create_manual_numbe_coords(matrix, c, r, &mut num_skips);
                part_numbers.push(number);
            }
        }
    }

    part_numbers
}

pub fn create_manual_numbe_coords(
    matrix: &Manual,
    c: usize,
    r: usize,
    num_skips: &mut usize,
) -> ManualNumberCoords {
    let number_coords = find_rest_of_number(matrix, c, r);

    *num_skips = number_coords.len();

    let surrounding_symbol_coords = get_symbol_coords_arround_coords(matrix, &number_coords);

    ManualNumberCoords {
        value: number_coords,
        surrounding_symbols: surrounding_symbol_coords,
    }
}

fn find_rest_of_number(matrix: &Manual, x: usize, y: usize) -> Vec<Coord> {
    let mut coords: Vec<Coord> = vec![];

    let mut x_mov = x;
    while is_in_bounds(&matrix, x_mov as i32, y as i32) && is_number(&matrix[y][x_mov]) {
        coords.push((x_mov, y));
        x_mov += 1;
    }

    coords
}

pub fn coords_to_num(matrix: &Manual, coords: &Vec<Coord>) -> i32 {
    let mut num_letters: Vec<&str> = coords
        .iter()
        .map(|(x, y)| &matrix[*y][*x])
        .map(String::as_str)
        .collect();

    let num_as_string = num_letters.join("");
    let parsed_num = num_as_string.parse::<i32>();

    if parsed_num.is_ok() {
        parsed_num.unwrap()
    } else {
        panic!("Number could not be parsed! [{}]", num_as_string);
    }
}

fn get_symbol_coords_arround_coords(matrix: &Manual, num_coords: &Vec<Coord>) -> Vec<Coord> {
    let mut all_sumbol_coords: Vec<Coord> = num_coords
        .iter()
        .flat_map(|coord| get_symbols_arround_coord(matrix, *coord))
        .filter(|(x, y)| !is_number(&matrix[*y][*x]))
        .collect();

    all_sumbol_coords.sort();
    all_sumbol_coords.dedup();

    all_sumbol_coords
}

fn get_symbols_arround_coord(matrix: &Manual, (x, y): Coord) -> Vec<Coord> {
    let possible_coords = get_possible_coords(x as i32, y as i32);

    possible_coords
        .iter()
        .filter(|(c, r)| is_in_bounds(matrix, *c, *r))
        .map(|(c, r)| (*c as usize, *r as usize))
        .collect()
}

fn get_possible_coords(x: i32, y: i32) -> Vec<(i32, i32)> {
    vec![
        (x - 1, y + 1),
        (x, y + 1),
        (x + 1, y + 1),
        (x - 1, y),
        (x + 1, y),
        (x - 1, y - 1),
        (x, y - 1),
        (x + 1, y - 1),
    ]
}

pub fn get_input(file_path: &str) -> String {
    println!("Reading file: {}", file_path);

    let contents = fs::read_to_string(file_path).expect("Should have been able to read the file");

    contents
}

pub fn format_to_vec(string: &String) -> Manual {
    let rows = vec_from_string(string, "\n");

    let vectorised: Manual = rows.iter().map(|row| vec_from_string(row, "")).collect();

    let res: Manual = vectorised
        .iter()
        .map(|row| row[1..row.len() - 1].to_vec())
        .collect();

    res
}

fn vec_from_string(string: &String, delimiter: &str) -> Vec<String> {
    string
        .trim()
        .split(delimiter)
        .map(|s| String::from(s))
        .collect()
}

pub fn number_coords_to_numbers(
    matrix: &Manual,
    number_coords: Vec<ManualNumberCoords>,
) -> Vec<ManualNumber> {
    number_coords
        .iter()
        .map(|num| to_manual_number(matrix, num))
        .collect()
}

pub fn to_manual_number(matrix: &Manual, number_coord: &ManualNumberCoords) -> ManualNumber {
    let value = coords_to_num(matrix, &number_coord.value);
    let surrounding_symbols: Vec<String> = number_coord
        .surrounding_symbols
        .iter()
        .map(|(x, y)| matrix[*y][*x].to_owned())
        .collect();

    ManualNumber {
        value,
        surrounding_symbols,
    }
}

fn print_matrix(mat: &Manual) {
    for row in mat {
        for ch in row {
            print!("{}|", ch);
        }
        println!();
    }
}

fn is_part_number(number: &ManualNumber) -> bool {
    number.surrounding_symbols.iter().any(|sym| is_symbol(sym))
}

pub fn is_number(string: &String) -> bool {
    string.parse::<i32>().is_ok()
}

fn is_in_bounds(matrix: &Manual, x: i32, y: i32) -> bool {
    matrix.len() as i32 > y && matrix[0].len() as i32 > x && y >= 0 && x >= 0
}

fn is_symbol(value: &String) -> bool {
    match value.as_str() {
        "." => false,
        _ => true,
    }
}
