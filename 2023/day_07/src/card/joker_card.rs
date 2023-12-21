use std::collections::HashMap;

use super::super::hand_type::{find_top_two, HandType};

use super::Card;

#[repr(u32)]
#[derive(Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Clone, Hash)]
pub enum JokerCard {
    A = 14,
    K = 13,
    Q = 12,
    Num(u32) = 11,
    J = 10,
}

impl Card for JokerCard {
    fn from_char(ch: char) -> Self {
        match ch {
            '9' | '8' | '7' | '6' | '5' | '4' | '3' | '2' => {
                JokerCard::Num(ch.to_digit(10).unwrap())
            }
            'T' => JokerCard::Num(10),
            'J' => JokerCard::J,
            'Q' => JokerCard::Q,
            'K' => JokerCard::K,
            'A' => JokerCard::A,
            _ => panic!("Unexpected card symbol: {ch}"),
        }
    }

    fn cards_from_string(str: &str) -> Vec<JokerCard> {
        str.chars().map(|c| JokerCard::from_char(c)).collect()
    }

    fn determine_hand_type(counts: &HashMap<Self, usize>) -> HandType
    where
        Self: Sized,
    {
        let jokers = counts.get(&JokerCard::J);
        let without_jokers = counts
            .into_iter()
            .filter(|(card, _)| **card != JokerCard::J)
            .map(|(card, count)| (*card, *count))
            .collect::<HashMap<JokerCard, usize>>();

        let top_two = find_top_two(&without_jokers);

        match jokers {
            None => HandType::from_counts(top_two.0, top_two.1),
            Some(n) => HandType::from_counts(top_two.0 + n, top_two.1), // Some(5) | Some(4) => HandType::FiveOfAKind,
                                                                        // Some(3) => match top_two {
                                                                        //     (2, _) => HandType::FiveOfAKind,
                                                                        //     _ => HandType::FourOfAKind,
                                                                        // },
                                                                        // Some(2) => match top_two {
                                                                        //     (3, _) => HandType::FiveOfAKind,
                                                                        //     (2, _) => HandType::FourOfAKind,
                                                                        //     _ => HandType::ThreeOfAKind,
                                                                        // },
                                                                        // Some(1) => match top_two {
                                                                        //
                                                                        // }
        }
    }
}

#[cfg(test)]
mod joker_card_tests {
    use super::JokerCard::*;
    use super::*;

    #[test]
    fn card_comps_work() {
        let seven = Num(7);
        let eight = Num(8);
        let king = K;
        let joker = J;

        assert!(joker < seven);
        assert!(king > eight);
    }

    #[test]
    fn from_char_works() {
        assert_eq!(J, JokerCard::from_char('J'));
        assert_eq!(Num(10), JokerCard::from_char('T'));
        assert_eq!(Num(8), JokerCard::from_char('8'));
    }

    #[test]
    fn cards_from_string_works() {
        let hand = "JAA8T";

        let expected_cards = vec![J, A, A, Num(8), Num(10)];

        let cards = JokerCard::cards_from_string(hand);

        assert_eq!(expected_cards, cards);
    }
}
