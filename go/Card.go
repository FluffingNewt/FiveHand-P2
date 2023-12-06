package main
import ( "fmt" )

// Represents a playing card.
// This struct defines a card object with a rank and suit.
// author: Davis Guest
type Card struct {
	rank int
	suit int
}


// Initializes a new card with the specified rank and suit.
func initCard(r int, s int) *Card {
	return &Card {
		rank : r,
		suit : s,
	}
}


// Returns a string representation of the card.
func (c *Card) toString() string {
	var suitLabel string
	if (c.suit == 0)        {suitLabel = "D"
	} else if (c.suit == 1) {suitLabel = "C"
	} else if (c.suit == 2) {suitLabel = "H"
	} else if (c.suit == 3) {suitLabel = "S"}

	var face string
	if (c.rank == 11)        {face = "J"
	} else if (c.rank == 12) {face = "Q"
	} else if (c.rank == 13) {face = "K"
	} else if (c.rank == 14) {face = "A"}

	if (face == "") {
		return fmt.Sprintf("%d%s", c.rank, suitLabel)
	} 
	return face + suitLabel
}


// Compares this card with another card based on their ranks.
func (c *Card) compare(other *Card) int {
	return c.rank - other.rank
}