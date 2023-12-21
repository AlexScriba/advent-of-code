use std::collections::HashMap;

use super::super::hand_type::{find_top_two, HandType};

use super::Card;

#[repr(u32)]
#[derive(Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Hash)]
pub enum NormalCard {
    A = 14,
    K = 13,
    Q = 12,
    J = 11,
    Num(u32) = 10,
}

impl Card for NormalCard {
    fn from_char(ch: char) -> NormalCard {
        match ch {
            '9' | '8' | '7' | '6' | '5' | '4' | '3' | '2' => {
                NormalCard::Num(ch.to_digit(10).unwrap())
            }
            'T' => NormalCard::Num(10),
            'J' => NormalCard::J,
            'Q' => NormalCard::Q,
            'K' => NormalCard::K,
            'A' => NormalCard::A,
            _ => panic!("Unexpected card symbol: {ch}"),
        }
    }

    fn cards_from_string(str: &str) -> Vec<NormalCard> {
        str.chars().map(|c| NormalCard::from_char(c)).collect()
    }

    fn determine_hand_type(counts: &HashMap<NormalCard, usize>) -> HandType {
        let top_two = find_top_two(counts);

        HandType::from_counts(top_two.0, top_two.1)
    }
}

#[cfg(test)]
mod normal_card_tests {
    use super::NormalCard::*;
    use super::*;

    #[test]
    fn from_char_works() {
        assert_eq!(NormalCard::Num(7), NormalCard::from_char('7'));
        assert_eq!(NormalCard::Num(10), NormalCard::from_char('T'));
        assert_eq!(NormalCard::K, NormalCard::from_char('K'));
    }

    #[test]
    fn card_comps_work() {
        let seven = NormalCard::Num(7);
        let eight = NormalCard::Num(8);
        let king = NormalCard::K;
        let ace = NormalCard::A;

        assert_eq!(seven < eight, true);
        assert_eq!(eight < king, true);
        assert_eq!(king < ace, true);
        assert_eq!(seven == eight, false);
    }

    #[test]
    fn cards_from_string_works() {
        let hand_string = "T4AKJ";

        let expected_cards = vec![Num(10), Num(4), A, K, J];

        let cards = NormalCard::cards_from_string(hand_string);

        assert_eq!(expected_cards, cards);
    }
}
