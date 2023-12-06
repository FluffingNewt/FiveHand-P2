include("Card.jl")
include("Deck.jl")
include("Hand.jl")

# Represents the game of Five Hand, a poker game with 6 hands.
# The game can be played with a randomized deck or a deck loaded from a file.
# author: Davis Guest
mutable struct FiveHand
    deck  :: Deck
    hands :: Vector{Hand}
end


# Initializes a new Five Hand game with a list of 6 empty hands and a deck of cards.
# If command line arguments are provided, it builds a file deck; otherwise, it builds a randomized deck.
function initFiveHand()
    deck = initDeck()
    hands = Hand[]

    file = length(ARGS) > 0 ? ARGS[1] : ""

    for i = 1:6
        push!(hands, initHand())
    end

    if (file != "") ; buildFileDeck(deck, file)
    else ; buildRandDeck(deck)
    end
    
    return FiveHand(deck, hands)
end


# Starts a Five Hand game.
function play(self::FiveHand, file::String)
    println("\n*** P O K E R   H A N D   A N A L Y Z E R ***\n")

        gameType = (file == "") ? 0 : 1

        if (gameType == 0) println( "\n*** USING RANDOMIZED DECK OF CARDS ***\n" *
                                    "\n*** Shuffled 52 card deck\n" *
                                    toString(self.deck) )

        else println( "\n*** USING TEST DECK ***\n" *
                      "\n*** File: " * file * "\n" *
                      toString(self.deck))
        end

        if (self.deck.duplicate != nothing)
            println( "\n*** ERROR - DUPLICATED CARD FOUND IN DECK ***\n" *
                     "\n*** DUPLICATE: " * toString(self.deck.duplicate) * " ***\n" )
            return
        end

        drawCards(self, gameType)

        println("\n*** Here are the six hands...")

        printAllHands(self)

        if (gameType == 0)
            println( "\n*** Here is what remains in the deck...\n" *
                    toString(self.deck) )
        end

        println("\n--- WINNING HAND ORDER ---")

        sortHands(self)
        printAllHands(self)
        println()
end


# Draws 30 cards to set up 6 hands of 5 cards.
# Alternates drawing cards among the hands.
function drawCards(self::FiveHand, gameType::Int)
    handNum = 1

    if (gameType == 0) 
        for i = 1:30
            if (handNum == 7) handNum = 1 ; end
            addCard(self.hands[handNum], drawCard(self.deck))
            handNum += 1
        end
    else
        for i = 1:30
            addCard(self.hands[handNum], drawCard(self.deck))
            if (i % 5 == 0) handNum += 1 ; end
        end
    end
end


# Prints all the hands to the console.
function printAllHands(self::FiveHand)
    for hand in self.hands
        println(toString(hand))
    end
end


# Sorts the hands to the winning order.
function sortHands(self::FiveHand)
    for j in 1:5, i in 1:5
        if (compare(self.hands[i], self.hands[i + 1]) < 0)
            temp              = self.hands[i + 1]
            self.hands[i + 1] = self.hands[i]
            self.hands[i]     = temp
        end
    end
end


# Main Method Calls
f = length(ARGS) > 0 ? ARGS[1] : ""
game = initFiveHand()
play(game, f)