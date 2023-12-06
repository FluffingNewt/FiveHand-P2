include("Card.jl")
import Random

# Representing a collection of a set of standard 52 playing cards, without a Joker.
# This class allows you to build a deck either randomly or from a file and provides
# methods to draw cards from the deck.
# author: Davis Guest
mutable struct Deck
    cards::Vector{Card}
    duplicate::Union{Card, Nothing}
    deckType::Int
end


# Initializes a new Deck with an empty list of cards and a default:
# - deck type of -1
# - duplicate of None
function initDeck()
    return Deck(Card[], nothing, -1)
end


# Returns a string representation of the deck.
function toString(self::Deck)
    list = ""
    
    for i = 1:length(self.cards)
        list *= self.cards[i].rank != 10 ? " " : ""
        
        list *= toString(self.cards[i])
        
        if i == length(self.cards)                           ; break
        elseif (i == 1 || i % 13 != 0) && self.deckType == 0 ; list *= ","
        elseif (i == 1 || i % 5  != 0) && self.deckType == 1 ; list *= ","
        else                                                 ; list *= "\n"
        end
    end
    
    return list
end


# Builds a random deck based on a standard deck of 52 playing cards without jokers.
# Shuffles the deck to randomize the card order.
function buildRandDeck(self::Deck)
    self.deckType = 0;

    for suit in 0:3, rank in 2:14
        push!(self.cards, initCard(rank, suit))
    end

    Random.shuffle!(self.cards);
end


# Builds a deck based on an input file.
function  buildFileDeck(self::Deck, fileName::String)
    self.deckType = 1
    file  = open(fileName, "r")
    lines = read(file, String)

    cardList = split(lines, [',','\n'], keepempty=false)

    for c in cardList
        i = (c[1] == ' ') ? 2 : 1
        
        rank = ifelse(c[i] == '1', 10, 
               ifelse(c[i] == 'J', 11,
               ifelse(c[i] == 'Q', 12,
               ifelse(c[i] == 'K', 13,
               ifelse(c[i] == 'A', 14,
                                   Int(c[i]) - Int('0') )))))

        suit = ifelse(c[3] == 'D', 0,
               ifelse(c[3] == 'C', 1,
               ifelse(c[3] == 'H', 2,
                                   3 )))
        
        for card in self.cards
            if card.rank == rank && card.suit == suit
                self.duplicate = card
            end
        end

        push!(self.cards, initCard(rank, suit))
    end 

    close(file)
end


# Draws a card from the deck, removing the first card.
function drawCard(self::Deck)
    return popfirst!(self.cards)
end