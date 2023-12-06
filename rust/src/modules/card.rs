// Represents a playing card.
// This class defines a card object with a rank and suit.
// author: Davis Guest
#[derive(Clone, Copy)]
pub struct Card {
    pub rank: i8,
    pub suit: i8,
}

impl Card {

    // Constructs a new card with the specified rank and suit.
    pub fn new(rank: i8, suit: i8) -> Self {
        return Card { rank, suit };
    }


    // Returns a string representation of the card.
    pub fn to_string(&self) -> String {
        let suit_label = match self.suit {
            0 => "D",
            1 => "C",
            2 => "H",
            3 => "S",
            _ => "",
        };
    
        let face = match self.rank {
            11 => "J",
            12 => "Q",
            13 => "K",
            14 => "A",
            _ => "",
        };
    
        if face.is_empty() {
            return format!("{}{}", self.rank, suit_label);
        }

        return format!("{}{}", face, suit_label);
    }

    
    // Compares this card with another card based on their ranks.
    pub fn compare(&self, other: &Card) -> i8 {
        return self.rank - other.rank;
    }
}