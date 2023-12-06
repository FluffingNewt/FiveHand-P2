package main
import (
	"math/rand"
	"time"
	"io/ioutil"
	"strconv"
	"strings"
)

// Representing a collection of a set of standard 52 playing cards, without a Joker.
// This class allows you to build a deck either randomly or from a file and provides
// methods to draw cards from the deck.
// author: Davis Guest
type Deck struct {
	cards []*Card
	duplicate *Card
	deckType int
}


// Initializes a new Deck with an empty list of cards and a default:
// - deck type of -1
// - duplicate of nil
func initDeck() *Deck {
	return &Deck {
		cards     : make([]*Card, 0),
		duplicate : nil,
		deckType  : -1,
	}
}


// Returns a string representation of the deck.
func (d *Deck) toString() string {
	var list string

	for i := 1; i <= len(d.cards); i++ {
		if (d.cards[i - 1].rank != 10) { list += " " }

		list += d.cards[i - 1].toString()

		if (i == len(d.cards))                                 { break
		} else if ((i == 0 || i % 13 != 0) && d.deckType == 0) { list += ","
		} else if ((i == 0 || i % 5 != 0) && d.deckType == 1)  { list += ","
		} else                                                 {list += "\n"}
	}

	return list

}


// Builds a random deck based on a standard deck of 52 playing cards without jokers.
// Shuffles the deck to randomize the card order.
func (d *Deck) buildRandDeck() {
	d.deckType = 0

	for suit := 0; suit <= 3; suit ++ {
		for rank := 2; rank <= 14; rank ++ {
			d.cards = append(d.cards, initCard(rank, suit))
		}
	}

	rand.Seed(time.Now().UnixNano())
	rand.Shuffle(len(d.cards), func(i, j int) {
        d.cards[i], d.cards[j] = d.cards[j], d.cards[i]
    })
}


// Builds a deck based on an input file.
func (d *Deck) buildFileDeck(file string) error {
	d.deckType = 1

	content, err := ioutil.ReadFile(file)
	if err != nil { return err }

	lines := strings.Split(string(content), "\n")

	for _, line := range lines {
		lineList := strings.Split(line, ",")

		for _, s := range lineList {
			i := 0
			if s[0] == ' ' { i = 1 }

			var rank int
			if s[i]        == '1' { rank = 10
			} else if s[i] == 'J' { rank = 11
			} else if s[i] == 'Q' { rank = 12
			} else if s[i] == 'K' { rank = 13
			} else if s[i] == 'A' { rank = 14
			} else {
				val, err := strconv.Atoi(string(s[i]))
				if err != nil { return err }
				rank = val
			}

			var suit int
			if s[2]        == 'D' { suit = 0
			} else if s[2] == 'C' { suit = 1
			} else if s[2] == 'H' { suit = 2
			} else if s[2] == 'S' { suit = 3 }

			for _, card := range d.cards {
				if card.rank == rank && card.suit == suit {
					d.duplicate = card
				}
			}

			d.cards = append(d.cards, initCard(rank, suit))

		}
	}

	return nil
}


// Draws a card from the deck, removing the first card.
func (d *Deck) drawCard() *Card {
	ret := d.cards[0]
	d.cards = d.cards[1:]
	return ret
}
