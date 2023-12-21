use std::collections::HashMap;
use std::hash::Hash;

use super::card::Card;

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Copy, Clone)]
pub enum HandType {
    FiveOfAKind = 7,
    FourOfAKind = 6,
    FullHouse = 5,
    ThreeOfAKind = 4,
    TwoPair = 3,
    OnePair = 2,
    HighCard = 1,
}

impl HandType {
    pub fn new<C>(cards: &Vec<C>) -> HandType
    where
        C: Card + Hash + Eq + Clone,
    {
        let count_map = count_card_ocurrences(cards);
        HandType::from_count_map(&count_map)
    }

    pub fn from_count_map<C>(map: &HashMap<C, usize>) -> HandType
    where
        C: Card,
    {
        C::determine_hand_type(map)
    }

    pub fn from_counts(highest: usize, second_highest: usize) -> HandType {
        match (highest, second_highest) {
            (5, _) => HandType::FiveOfAKind,
            (4, _) => HandType::FourOfAKind,
            (3, 2) => HandType::FullHouse,
            (3, 1) => HandType::ThreeOfAKind,
            (2, 2) => HandType::TwoPair,
            (2, 1) => HandType::OnePair,
            _ => HandType::HighCard,
        }
    }
}

pub fn find_top_two<C>(map: &HashMap<C, usize>) -> (usize, usize) {
    let mut nums = map.iter().map(|(_, v)| *v).collect::<Vec<usize>>();

    match nums.len() {
        0 => (0, 0),
        1 => (nums[0], 0),
        _ => {
            nums.sort_unstable();
            nums.reverse();

            (nums[0], nums[1])
        }
    }
}

fn count_card_ocurrences<C>(cards: &Vec<C>) -> HashMap<C, usize>
where
    C: Hash + Eq + Clone,
{
    if cards.len() < 5 {
        panic!("Not enough cards for a hand.")
    }

    let mut map: HashMap<C, usize> = HashMap::new();

    for card in cards {
        let card_count = map.entry(card.to_owned()).or_insert(0);
        *card_count += 1;
    }
    map
}

#[cfg(test)]
mod hand_type_tests {
    use super::super::card::joker_card::JokerCard;

    use super::super::card::normal_card::NormalCard;
    use super::*;

    #[test]
    fn ord_between_types() {
        let lowest_type = HandType::HighCard;
        let highest_type = HandType::FiveOfAKind;

        assert_eq!(lowest_type < highest_type, true);
    }

    #[test]
    fn type_creation() {
        let cards = vec![
            NormalCard::K,
            NormalCard::J,
            NormalCard::K,
            NormalCard::J,
            NormalCard::J,
        ];
        let expected_hand_type = HandType::FullHouse;

        let hand_type = HandType::new(&cards);

        assert_eq!(expected_hand_type, hand_type);
    }

    #[test]
    fn counts_cards_correctly() {
        let cards = vec![
            NormalCard::A,
            NormalCard::K,
            NormalCard::A,
            NormalCard::Num(2),
            NormalCard::Num(7),
        ];

        let expected_result = HashMap::from([
            (NormalCard::A, 2),
            (NormalCard::K, 1),
            (NormalCard::Num(2), 1),
            (NormalCard::Num(7), 1),
        ]);

        let result = count_card_ocurrences(&cards);

        assert_eq!(expected_result, result);
    }

    #[test]
    fn counts_to_five_of_a_kind() {
        let map = HashMap::from([(NormalCard::A, 5)]);
        let expected_result = HandType::FiveOfAKind;

        let result = HandType::from_count_map(&map);

        assert_eq!(expected_result, result);
    }

    #[test]
    fn counts_to_four_of_a_kind() {
        let map = HashMap::from([(NormalCard::A, 1), (NormalCard::K, 4)]);
        let expected_result = HandType::FourOfAKind;

        let result = HandType::from_count_map(&map);

        assert_eq!(expected_result, result);
    }

    #[test]
    fn counts_to_full_house() {
        let map = HashMap::from([(NormalCard::A, 3), (NormalCard::K, 2)]);
        let expected_result = HandType::FullHouse;

        let result = HandType::from_count_map(&map);

        assert_eq!(expected_result, result);
    }

    #[test]
    fn counts_to_three_of_a_kind() {
        let map = HashMap::from([(NormalCard::A, 3), (NormalCard::K, 1), (NormalCard::J, 1)]);
        let expected_result = HandType::ThreeOfAKind;

        let result = HandType::from_count_map(&map);

        assert_eq!(expected_result, result);
    }

    #[test]
    fn counts_to_two_pair() {
        let map = HashMap::from([
            (NormalCard::A, 2),
            (NormalCard::K, 1),
            (NormalCard::Num(4), 2),
        ]);
        let expected_result = HandType::TwoPair;

        let result = HandType::from_count_map(&map);

        assert_eq!(expected_result, result);
    }

    #[test]
    fn counts_to_one_pair() {
        let map = HashMap::from([
            (NormalCard::A, 1),
            (NormalCard::K, 1),
            (NormalCard::J, 2),
            (NormalCard::Num(2), 1),
        ]);
        let expected_result = HandType::OnePair;

        let result = HandType::from_count_map(&map);

        assert_eq!(expected_result, result);
    }

    #[test]
    fn counts_to_high_card() {
        let map = HashMap::from([
            (NormalCard::A, 1),
            (NormalCard::K, 1),
            (NormalCard::Q, 1),
            (NormalCard::Num(5), 1),
            (NormalCard::Num(7), 1),
        ]);
        let expected_result = HandType::HighCard;

        let result = HandType::from_count_map(&map);

        assert_eq!(expected_result, result);
    }

    #[test]
    fn joker_card_works() {
        let map = HashMap::from([
            (JokerCard::A, 1),
            (JokerCard::K, 1),
            (JokerCard::Q, 1),
            (JokerCard::J, 2),
        ]);

        let expected_result = HandType::ThreeOfAKind;

        let result = HandType::from_count_map(&map);

        assert_eq!(expected_result, result);
    }
}
