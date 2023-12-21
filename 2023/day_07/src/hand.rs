use std::cmp::Ordering;
use std::hash::Hash;

use super::card::Card;
use super::hand_type::HandType;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct HandCards<C>(C, C, C, C, C);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Hand<C> {
    pub cards: HandCards<C>,
    pub hand_type: HandType,
    pub bid: u32,
}

impl<C> Hand<C>
where
    C: Card + Clone + Hash + Eq + Copy,
{
    pub fn new(bid: u32, cards: &Vec<C>) -> Hand<C> {
        if cards.len() < 5 {
            panic!("Too few cards for hand");
        }

        let hand_type = HandType::new(cards);
        let hand_cards = HandCards(cards[0], cards[1], cards[2], cards[3], cards[4]);

        Hand {
            cards: hand_cards,
            hand_type,
            bid,
        }
    }
}

impl<C> PartialOrd for Hand<C>
where
    C: Ord,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<C> Ord for Hand<C>
where
    C: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        if self.hand_type != other.hand_type {
            return self.hand_type.cmp(&other.hand_type);
        }

        self.cards.cmp(&other.cards)
    }
}

#[cfg(test)]
mod hand_tests {
    use super::super::card::normal_card::NormalCard::*;
    use super::*;

    #[test]
    fn creation_works() {
        let cards = vec![A, Q, Q, A, A];
        let bid = 100;

        let expected_hand = Hand {
            cards: HandCards(A, Q, Q, A, A),
            hand_type: HandType::FullHouse,
            bid: 100,
        };

        let hand = Hand::new(bid, &cards);

        assert_eq!(expected_hand, hand);
    }

    #[test]
    fn ord_works_on_different_hand_types() {
        let hand_1 = Hand {
            cards: HandCards(A, A, A, A, A),
            hand_type: HandType::FiveOfAKind,
            bid: 10,
        };

        let hand_2 = Hand {
            cards: HandCards(Num(3), A, A, A, A),
            hand_type: HandType::FourOfAKind,
            bid: 1000,
        };

        assert!(hand_1 > hand_2);
    }

    #[test]
    fn ord_works_on_same_hand_types() {
        let hand_1 = Hand {
            cards: HandCards(A, A, A, Num(3), A),
            hand_type: HandType::FourOfAKind,
            bid: 10,
        };

        let hand_2 = Hand {
            cards: HandCards(A, Num(3), A, A, A),
            hand_type: HandType::FourOfAKind,
            bid: 1000,
        };

        assert!(hand_1 > hand_2);
    }
}
