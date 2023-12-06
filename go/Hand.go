package main
import ("sort")

// Represents a hand of playing cards.
// This class defines a hand object that can hold a collection of card objects.
// author: Davis Guest
type Hand struct {
	cards []*Card
	sorted []*Card
	handType int
}


// Initializes a new hand with an empty list of cards, its sorted variant, and a default hand type of 0.
func initHand() *Hand {
	return &Hand {
		cards    : make([]*Card, 0),
		sorted   : make([]*Card, 0),
		handType : 0,
	}
}


// Adds a card to the hand's list of cards.
func (h *Hand) addCard(card *Card) {
	h.cards = append(h.cards, card)
}


// Returns a string representation of the hand.
func (h *Hand) toString() string {
	list := ""

	for i := 1; i <= len(h.cards); i++ {
		if (h.cards[i - 1].rank != 10) { list += " " }

		list += h.cards[i - 1].toString()

		if (i == 1 || i % 5 != 0) { list += " " }
	}

	if h.handType == 0         { return list
	} else if h.handType == 10 { list += " - Royal Straight Flush"
	} else if h.handType == 9  { list += " - Straight Flush"
	} else if h.handType == 8  { list += " - Four of a Kind"
	} else if h.handType == 7  { list += " - Full House"
	} else if h.handType == 6  { list += " - Flush"
	} else if h.handType == 5  { list += " - Straight"
	} else if h.handType == 4  { list += " - Three of a Kind"
	} else if h.handType == 3  { list += " - Two Pair"
	} else if h.handType == 2  { list += " - Pair"
	} else                     { list += " - High Card" }

	return list
}


// Compares this hand with another hand based on their ranks.
// Used to sort each hand type and tiebreakers accordingly.
func (h *Hand) compare(other *Hand) int {
	h.assessHand()
	other.assessHand()

	typeComparison := h.handType - other.handType

	return h.compareHelper(other, typeComparison, 0)
}


// Recursive helper method for the compare method.
func (h *Hand) compareHelper(other *Hand, diff int, pass int) int {
	if (diff != 0) { return diff }

	thisBreaker := h.getTieBreakerCard(pass)
	otherBreaker := other.getTieBreakerCard(pass)

	rankDiff := thisBreaker.rank - otherBreaker.rank
	suitDiff := thisBreaker.suit - otherBreaker.suit

	if (pass < 2 && (h.handType == 2 || h.handType == 3)) {
		if (rankDiff == 0) {
			return h.compareHelper(other, rankDiff, pass + 1)
		}
		return rankDiff
	}

	if (rankDiff == 0) { return suitDiff }
	return rankDiff
}


// Analyzes the current collection of cards in the hand and determines its hand type.
// The sorted instance variable is updated to contain the cards sorted by rank.
// This method sets the handType instance variable.
func (h *Hand) assessHand() {
	h.sortHand()

	if (h.isRoyalStraightFlush())   { h.handType = 10
	} else if (h.isStraightFlush()) { h.handType = 9
	} else if (h.isFourOfAKind())   { h.handType = 8
	} else if (h.isFullHouse())     { h.handType = 7
	} else if (h.isFlush())         { h.handType = 6
	} else if (h.isStraight())      { h.handType = 5
	} else if (h.isThreeOfAKind())  { h.handType = 4
	} else if (h.isTwoPair())       { h.handType = 3
	} else if (h.isPair())          { h.handType = 2
	} else                          { h.handType = 1 }
}


// Determines if the hand is a royal straight flush.
func (h *Hand) isRoyalStraightFlush() bool {
	rankList := h.getRankList()

	return h.isStraightFlush() &&
		   rankList[0] == 10   &&
		   rankList[4] == 14
}


// Determines if the hand is a straight flush.
func (h *Hand) isStraightFlush() bool {
	return h.isStraight() && h.isFlush()
}


// Determines if the hand is a four of a kind.
func (h *Hand) isFourOfAKind() bool {
	rankList := h.getRankList()

	return (rankList[0] == rankList[3]) ||
		   (rankList[1] == rankList[4])
}


// Determines if the hand is a full house.
func (h *Hand) isFullHouse() bool {
	rankList := h.getRankList()

	return (rankList[0] == rankList[1] && rankList[2] == rankList[4]) ||
		   (rankList[0] == rankList[2] && rankList[3] == rankList[4])
}


// Determines if the hand is a flush.
func (h *Hand) isFlush() bool {
	suit := h.sorted[0].suit

	for i := 0; i < 4; i++ {
		if h.sorted[i].suit != suit {
			return false
		}
	}

	return h.sorted[4].suit == suit
}


// Determines if the hand is a straight.
func (h *Hand) isStraight() bool {
	rankList := h.getRankList()

	if rankList[4] == 14 && rankList[0] == 2 {
		rankList[4] = 1
		sort.Ints(rankList)
	}

	for i := 0; i < 4; i++ {
		if rankList[i+1] != rankList[i]+1 {
			return false
		}
	}

	return true
}


// Determines if the hand is a three of a kind.
func (h *Hand) isThreeOfAKind() bool {
	rankList := h.getRankList()

	return (rankList[0] == rankList[2]) ||
		   (rankList[1] == rankList[3]) ||
		   (rankList[2] == rankList[4])
}


// Determines if the hand is a two pair.
func (h *Hand) isTwoPair() bool {
	rankList := h.getRankList()

	return (rankList[0] == rankList[1] && rankList[2] == rankList[3]) ||
		   (rankList[0] == rankList[1] && rankList[3] == rankList[4]) ||
		   (rankList[1] == rankList[2] && rankList[3] == rankList[4])
}


// Determines if the hand is a pair.
func (h *Hand) isPair() bool {
	rankList := h.getRankList()

	for i := 0; i < 4; i++ {
		if rankList[i] == rankList[i+1] {
			return true
		}
	}

	return false
}


// Determines the tie breaking card of the hand depending on its handType.
func (h *Hand) getTieBreakerCard(pass int) *Card {
	if h.handType == 10 { return h.sorted[4]

	} else if h.handType == 9 {
		if h.sorted[4].rank == 14 && h.sorted[0].rank == 2 {
			return h.sorted[3]
		}
		return h.sorted[4]

	} else if h.handType == 8 { return h.sorted[2]
	} else if h.handType == 7 { return h.sorted[2]
	} else if h.handType == 6 { return h.sorted[4]
	} else if h.handType == 5 { return h.sorted[4]
	} else if h.handType == 4 { return h.sorted[2]

	} else if h.handType == 3 {
		pairList := make([]*Card, 0)

		for i := 1; i < len(h.sorted); i++ {
			currentCard  := h.sorted[i]
			previousCard := h.sorted[i - 1]

			if currentCard.rank == previousCard.rank {
				pairList = append(pairList, currentCard)
			}
		}

		max := pairList[0]
		min := pairList[0]

		for _, card := range pairList {
			if card.rank > max.rank { max = card }
			if card.rank < min.rank { min = card }
		}

		if pass == 0        { return max
		} else if pass == 1 { return min
		} else              { return h.getKicker() }

	} else if h.handType == 2 {
		var ret *Card = nil

		for i := 1; i < len(h.sorted); i++ {
			currentCard  := h.sorted[i]
			previousCard := h.sorted[i - 1]

			if currentCard.rank == previousCard.rank {
				ret = currentCard
				break
			}
		}

		if pass == 0 { return ret
		} else if pass == 1 { return h.getKicker() }
	}

	return h.sorted[4]
}

// Helper method to get a sorted list of ranks in the hand.
func (h *Hand) getRankList() []int {
	rankList := make([]int, 0)

	for i := 0; i < len(h.cards); i++ {
		rankList = append(rankList, h.cards[i].rank)
	}

	sort.Ints(rankList)

	return rankList;
}

// Gets the kicker card for pairs and two pairs.
func (h *Hand) getKicker() *Card {
	nonPairList := make([]*Card, 0)

	for i := 1; i < len(h.sorted); i++ {
		currentCard  := h.sorted[i]
		previousCard := h.sorted[i - 1]

		if currentCard.rank != previousCard.rank {
			nonPairList = append(nonPairList, currentCard)
		}
	}

	max := nonPairList[0]

	for _, card := range nonPairList {
		if card.rank > max.rank { max = card }
	}

	return max
}


// Sets the sorted instance variable to a sorted version of a provided hand.
func (h *Hand) sortHand() {

	for i := 0; i < 5; i++ {
		h.sorted = append(h.sorted, initCard(h.cards[i].rank, h.cards[i].suit))
	}
	
	for j := 0; j < 4; j++ {
		for i := 0; i < 4; i++ {
			if h.sorted[i].compare(h.sorted[i + 1]) > 0 {
				temp := initCard(h.sorted[i + 1].rank, h.sorted[i + 1].suit)
				h.sorted[i + 1] = h.sorted[i]
				h.sorted[i] = temp
			}
		}
	}
}