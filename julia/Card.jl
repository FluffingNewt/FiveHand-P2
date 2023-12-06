# Represents a playing card.
# This class defines a card object with a rank and suit.
# author: Davis Guest
mutable struct Card
    rank::Int
    suit::Int
end

# Initializes a new card with the specified rank and suit.
function initCard(r::Int, s::Int)
    return Card(r, s)
end


# Returns a string representation of the card.
function toString(self::Card)
    suitLabel = ifelse(self.suit == 0, "D",
                ifelse(self.suit == 1, "C",
                ifelse(self.suit == 2, "H",
                                       "S" )))
                
    face = ifelse(self.rank == 11, "J",
           ifelse(self.rank == 12, "Q",
           ifelse(self.rank == 13, "K",
           ifelse(self.rank == 14, "A",
                                   "" ))))

    if isempty(face) return string(self.rank, suitLabel)
    else return string(face, suitLabel)
    end
end


# Compares this card with another card based on their ranks.
function compare(self::Card, other::Card)
    return self.rank - other.rank
end