use std::collections::HashMap;

use super::hand_type::HandType;

pub mod joker_card;
pub mod normal_card;

pub trait Card {
    fn from_char(ch: char) -> Self;
    fn cards_from_string(str: &str) -> Vec<Self>
    where
        Self: Sized;

    fn determine_hand_type(counts: &HashMap<Self, usize>) -> HandType
    where
        Self: Sized;
}
