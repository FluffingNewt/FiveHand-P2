package main
import (
	"fmt"
	"os"
)

// Represents the game of Five Hand, a poker game with 6 hands.
// The game can be played with a randomized deck or a deck loaded from a file.
// author: Davis Guest
type FiveHand struct {
	deck *Deck
	hands []*Hand
}


// Initializes a new Five Hand game with a list of 6 empty hands and a deck of cards.
// If command line arguments are provided, it builds a file deck; otherwise, it builds a randomized deck.
func initFiveHand(file string) *FiveHand {
	
	var d = initDeck()
	var h = make([]*Hand, 0)

	for i := 0; i < 6; i++ {
		h = append(h, initHand())
	}

	if (file != "") { d.buildFileDeck(file) 
	} else          { d.buildRandDeck() }

	return &FiveHand {
		deck  : d,
		hands : h,
	}
}


// Starts a Five Hand game.
func (f *FiveHand) play(file string) {

	fmt.Println("\n*** P O K E R   H A N D   A N A L Y Z E R ***\n")

	gameType := 0
	if (file != "") { gameType = 1 }

	if (gameType == 0) { fmt.Println( "\n*** USING RANDOMIZED DECK OF CARDS ***\n" + 
									  "\n*** Shuffled 52 card deck\n" +
									  f.deck.toString())

	} else { fmt.Println( "\n*** USING TEST DECK ***\n" +
						  "\n*** File: " + file + "\n" +
						  f.deck.toString()) }

	if (f.deck.duplicate != nil) { fmt.Println( "\n*** ERROR - DUPLICATED CARD FOUND IN DECK ***\n" +
												"\n*** DUPLICATE: " + f.deck.duplicate.toString() + " ***\n")
												return }

	f.drawCards(gameType)

	fmt.Println("\n*** Here are the six hands...")

	f.printAllHands()

	if (gameType == 0) { fmt.Println( "\n*** Here is what remains in the deck...\n" +
									  f.deck.toString()) }

	fmt.Println("\n--- WINNING HAND ORDER ---")

	f.sortHands()
	f.printAllHands()
	fmt.Println()
}


// Draws 30 cards to set up 6 hands of 5 cards.
func (f *FiveHand) drawCards(gameType int) {
	handNum := 0
	if (gameType == 0) {
		for i := 0; i < 30; i++ {
			if (handNum == 6) { handNum = 0 }
			f.hands[handNum].addCard(f.deck.drawCard())
			handNum++
		}
	} else {
		for i := 1; i <= 30; i++ {
			f.hands[handNum].addCard(f.deck.drawCard())
			if (i % 5 == 0) { handNum++ }
		}
	}
}


// Prints all the hands to the console.
func (f *FiveHand) printAllHands() {
	for i := 0; i < len(f.hands); i++ {
		fmt.Println(f.hands[i].toString())
	}
}


// Sorts the hands to the winning order.
func (f *FiveHand) sortHands() {
	for j := 0; j < len(f.hands) - 1; j++ {
        for i := 0; i < len(f.hands) - 1; i++ {
            if f.hands[i].compare(f.hands[i + 1]) < 0 {
                temp          := f.hands[i + 1]
                f.hands[i + 1] = f.hands[i]
                f.hands[i]     = temp
            }
        }
    }
}


// Main Method Calls
func main() {
	f := ""
	if (len(os.Args) > 1) { f = os.Args[1] }
	game := initFiveHand(f)
	game.play(f)
}