use crate::modules::card::Card;

// Represents a hand of playing cards.
// This class defines a hand object that can hold a collection of card objects.
// author: Davis Guest
#[derive(Clone)]
pub struct Hand {
    pub cards: Vec<Card>,
    pub sorted: Vec<Card>,
    pub hand_type: i8,
}

impl Hand {

    // Consutructs a new hand with an empty list of cards, its sorted variant, and a default hand type of 0.
    pub fn new() -> Self {
        return Hand { cards: Vec::new(), sorted: Vec::new(), hand_type: 0 };
    }


    // Returns a string representation of the hand.
    pub fn to_string(&self) -> String {
        let mut list = String::new();

        for i in 1..=5 {
            if self.cards[i - 1].rank != 10 { list += " "; }

            list += &self.cards[i - 1].to_string();

            if i == 1 || i % 5 != 0 { list += " "; }
        }

        if self.hand_type == 0         { return list;
        } else if self.hand_type == 10 { list += " - Royal Straight Flush";
        } else if self.hand_type == 9  { list += " - Straight Flush";
        } else if self.hand_type == 8  { list += " - Four of a Kind";
        } else if self.hand_type == 7  { list += " - Full House";
        } else if self.hand_type == 6  { list += " - Flush";
        } else if self.hand_type == 5  { list += " - Straight";
        } else if self.hand_type == 4  { list += " - Three of a Kind";
        } else if self.hand_type == 3  { list += " - Two Pair";
        } else if self.hand_type == 2  { list += " - Pair";
        } else                         { list += " - High Card"; }

        return list;
    }


    // Adds a card to the hand's list of cards.
    pub fn add_card(&mut self, card: Card) {
        self.cards.push(card);
    }


    // Compares this hand with another hand based on their ranks.
    // Used to sort each hand type and tiebreakers accordingly.
    pub fn compare(&mut self, mut other: Hand) -> i8 {
        self.assess_hand();
        other.assess_hand();

        let type_comparison = self.hand_type - other.hand_type;

        return self.compare_helper(other, type_comparison, 0);
    }


    // Recursive helper method for the compare_hand method.
    pub fn compare_helper(&mut self, other: Hand, diff: i8, pass: i8) -> i8 {
        if diff != 0 { return diff; }

        let this_breaker = self.get_tiebreaker_card(pass);
        let other_breaker = other.get_tiebreaker_card(pass);

        let rank_diff = this_breaker.rank - other_breaker.rank;
        let suit_diff = this_breaker.suit - other_breaker.suit;

        if pass < 2 && (self.hand_type == 2 || self.hand_type == 3) {
            if rank_diff == 0 {
                return self.compare_helper(other, rank_diff, pass + 1);
            }
                
            return rank_diff
        }

        if rank_diff == 0 { return suit_diff; }
        return rank_diff;
    }


    // Analyzes the current collection of cards in the hand and determines its hand type.
    // The sorted instance variable is updated to contain the cards sorted by rank.
    // This method sets the hand_type instance variable
    pub fn assess_hand(&mut self) {
        self.sort_hand();

        if        self.is_royal_straight_flush() { self.hand_type = 10;
        } else if self.is_straight_flush()       { self.hand_type = 9;
        } else if self.is_four_of_a_kind()       { self.hand_type = 8;
        } else if self.is_full_house()           { self.hand_type = 7;
        } else if self.is_flush()                { self.hand_type = 6;
        } else if self.is_straight()             { self.hand_type = 5;
        } else if self.is_three_of_a_kind()      { self.hand_type = 4;
        } else if self.is_two_pair()             { self.hand_type = 3;
        } else if self.is_pair()                 { self.hand_type = 2;
        } else                                   { self.hand_type = 1; }
    }


    // Determines if the hand is a royal straight flush.
    fn is_royal_straight_flush(&self) -> bool {
        let rank_list = self.get_rank_list();

        return self.is_straight_flush() &&
                     rank_list[0] == 10 &&
                     rank_list[4] == 14;
    }


    // Determines if the hand is a straight flush.
    fn is_straight_flush(&self) -> bool {
        return self.is_straight() && self.is_flush();
    }

    // Determines if the hand is four of a kind.
    fn is_four_of_a_kind(&self) -> bool {
        let rank_list = self.get_rank_list();

        return rank_list[0] == rank_list[3] ||
               rank_list[1] == rank_list[4];
    }


    // Determines if the hand is a full house.
    fn is_full_house(&self) -> bool {
        let rank_list = self.get_rank_list();

        return (rank_list[0] == rank_list[1] && rank_list[2] == rank_list[4]) ||
               (rank_list[0] == rank_list[2] && rank_list[3] == rank_list[4]);
    }


    // Determines if the hand is a flush.
    fn is_flush(&self) -> bool {
        let suit = self.sorted[0].suit;

        for i in 0..=4 {
            if self.sorted[i].suit != suit {
                return false;
            }
        }

        return self.sorted[4].suit == suit;
    }


    // Determines if the hand is a straight.
    fn is_straight(&self) -> bool {
        let mut rank_list = self.get_rank_list();

        if rank_list[4] == 14 && rank_list[0] == 2 {
            rank_list[4] = 1;
            rank_list.sort();
        }

        for i in 0..4 {
            if rank_list[i + 1] != rank_list[i] + 1 {
                return false;
            }
        }

        return true;
    }


    // Determines if the hand is three of a kind.
    fn is_three_of_a_kind(&self) -> bool {
        let rank_list = self.get_rank_list();

        return rank_list[0] == rank_list[2] ||
               rank_list[1] == rank_list[3] ||
               rank_list[2] == rank_list[4];
    }


    // Determines if the hand is two pair.
    fn is_two_pair(&self) -> bool {
        let rank_list = self.get_rank_list();

        return (rank_list[0] == rank_list[1] && rank_list[2] == rank_list[3]) ||
               (rank_list[0] == rank_list[1] && rank_list[3] == rank_list[4]) ||
               (rank_list[1] == rank_list[2] && rank_list[3] == rank_list[4]);
    }


    // Determines if the hand is a pair.
    fn is_pair(&self) -> bool {
        let rank_list = self.get_rank_list();

        for i in 0..4 {
            if rank_list[i] == rank_list[i + 1] {
                return true;
            }
        }

        return false;
    }


    // Determines the tie breaking card of the hand depending on its handType.
    pub fn get_tiebreaker_card(&self, pass: i8) -> Card {
        if self.hand_type == 10 { return self.sorted[4];

        } else if self.hand_type == 9 {
            if self.sorted[4].rank == 14 && self.sorted[0].rank == 2 {
                return self.sorted[3];
            }
            return self.sorted[4];

        } else if self.hand_type == 8 { return self.sorted[2];
        } else if self.hand_type == 7 { return self.sorted[2];
        } else if self.hand_type == 6 { return self.sorted[4];
        } else if self.hand_type == 5 { return self.sorted[4];
        } else if self.hand_type == 4 { return self.sorted[2];

        } else if self.hand_type == 3 {
            let mut pair_list = Vec::<Card>::new();
    
            for i in 1..5 {
                let current_card = self.sorted[i];
                let previous_card = self.sorted[i - 1];
    
                if current_card.compare(&previous_card) == 0 {
                    pair_list.push(current_card);
                }
            }
    
            let mut max = pair_list[0];
            let mut min = pair_list[0];
    
            for card in pair_list {
                if card.rank > max.rank { max = card; }
                if card.rank < min.rank { min = card; }
            }
    
            if        pass == 0 { return max;
            } else if pass == 1 { return min;
            } else              { return self.get_kicker(); }

        } else if self.hand_type == 2 {
            let mut ret = self.sorted[0];
    
            for i in 1..5 {
                let current_card = self.sorted[i];
                let previous_card = self.sorted[i - 1];
    
                if current_card.compare(&previous_card) == 0 {
                    ret = current_card;
                    break;
                }
            }
    
            if        pass == 0 { return ret;
            } else if pass == 1 { return self.get_kicker(); }
        }
    
        return self.sorted[4];
    }


    // Gets the kicker card for pairs and two pairs.
    pub fn get_kicker(&self) -> Card {
        let mut non_pair_list: Vec<Card> = Vec::new();

        for i in 1..4 {
            let current_card = self.sorted[i];
            let previous_card = self.sorted[i - 1];

            if current_card.rank != previous_card.rank {
                non_pair_list.push(current_card);
            }
        }

        let mut max = non_pair_list[0];

        for card in non_pair_list {
            if card.rank > max.rank {
                max = card;
            }
        }

        return max;
    }


    // Helper method to get a sorted list of ranks in the hand.
    pub fn get_rank_list(&self) -> Vec<i8> {
        let mut rank_list: Vec<i8> = Vec::new();

        for card in &self.cards {
            rank_list.push(card.rank);
        }

        rank_list.sort_unstable();

        return rank_list;
    }


    // Sets the sorted instance variable to a sorted version of a provided hand.
    pub fn sort_hand(&mut self) {
        for i in 0..=4 {
            self.sorted.push(Card::new(self.cards[i].rank, self.cards[i].suit));
        }

        for _j in 0..=3 {
            for i in 0..=3 {
                if self.sorted[i].compare(&self.sorted[i + 1]) > 0 {
                    let temp           = Card::new(self.sorted[i + 1].rank, self.sorted[i + 1].suit);
                    self.sorted[i + 1] = self.sorted[i];
                    self.sorted[i]     = temp;
                }
            }
        }
    }
}