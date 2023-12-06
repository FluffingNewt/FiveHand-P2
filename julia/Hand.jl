include("Card.jl")

# Represents a hand of playing cards.
# This class defines a hand object that can hold a collection of card objects.
# author: Davis Guest
mutable struct Hand
    cards    :: Vector{Card}
    sorted   :: Vector{Card}
    handType :: Int
end


# Initializes a new hand with an empty list of cards, its sorted variant, and a default hand type of 0.
function initHand()
    return Hand(Card[], Card[], 0)
end


# Returns a string representation of the hand.
function toString(self::Hand)
    str = ""
    for i = 1:5
        if (self.cards[i].rank != 10) ; str *= " " ; end

        str *= toString(self.cards[i])

        if (i == 1 || i % 5 != 0) ; str *= " " ; end
    end

    if (self.handType == 0)      ; return str
    elseif (self.handType == 10) ; str *= " - Royal Straight Flush"
    elseif (self.handType == 9)  ; str *= " - Straight Flush"
    elseif (self.handType == 8)  ; str *= " - Four of a Kind"
    elseif (self.handType == 7)  ; str *= " - Full House"
    elseif (self.handType == 6)  ; str *= " - Flush"
    elseif (self.handType == 5)  ; str *= " - Straight"
    elseif (self.handType == 4)  ; str *= " - Three of a Kind"
    elseif (self.handType == 3)  ; str *= " - Two Pair"
    elseif (self.handType == 2)  ; str *= " - Pair"
    else                         ; str *= " - High Card"
    end

    return str;
end

# Adds a card to the hand's list of cards.
function addCard(self::Hand, card::Card)
    push!(self.cards, card)
end


# Compares this hand with another hand based on their ranks.
# Used to sort each hand type and tiebreakers accordingly.
function compare(self::Hand, other::Hand)
    assessHand(self) , assessHand(other)

    typeComparison = self.handType - other.handType

    return compareHelper(self, other, typeComparison, 0)
end


# Recursive helper method for the compare_hand method.
function compareHelper(self::Hand, other::Hand, diff::Int, pass::Int)
    diff != 0 && return diff

    thisBreaker = getTieBreakerCard(self, pass)
    otherBreaker = getTieBreakerCard(other, pass)

    rankDiff = thisBreaker.rank - otherBreaker.rank
    suitDiff = thisBreaker.suit - otherBreaker.suit

    if (pass < 2 && (self.handType == 2 || self.handType == 3))
        rankDiff == 0 && return compareHelper(self, other, rankDiff, pass + 1)
        return rankDiff
    end

    rankDiff == 0 && return suitDiff
    return rankDiff;
end


# Analyzes the current collection of cards in the hand and determines its hand type.
# The sorted instance variable is updated to contain the cards sorted by rank.
# This method sets the handType instance variable.
function assessHand(self::Hand)
    self.sorted = sortHand(self.cards)

    if     isRoyalStraightFlush(self) ; self.handType = 10
    elseif isStraightFlush(self)      ; self.handType = 9
    elseif isFourOfAKind(self)        ; self.handType = 8
    elseif isFullHouse(self)          ; self.handType = 7
    elseif isFlush(self)              ; self.handType = 6
    elseif isStraight(self)           ; self.handType = 5
    elseif isThreeOfAKind(self)       ; self.handType = 4
    elseif isTwoPair(self)            ; self.handType = 3
    elseif isPair(self)               ; self.handType = 2
    else                              ; self.handType = 1
    end
end


# Determines if the hand is a royal straight flush.
function isRoyalStraightFlush(self::Hand)
    rankList = getRankList(self)

    return isStraight(self) && rankList[1] == 10 && rankList[5] == 14
end


# Determines if the hand is a straight flush.
function isStraightFlush(self::Hand)
    rankList = getRankList(self)

    return isStraight(self) && isFlush(self)
end


# Determines if the hand is a four of a kind.
function isFourOfAKind(self::Hand)
    rankList = getRankList(self)

    return (rankList[1] == rankList[4]) || (rankList[2] == rankList[5])
end


# Determines if the hand is a full house.
function isFullHouse(self::Hand)
    rankList = getRankList(self)

    return ((rankList[1] == rankList[2]) && (rankList[3] == rankList[5])) &&
           ((rankList[1] == rankList[3]) && (rankList[4] == rankList[5]))
end


# Determines if the hand is a flush.
function isFlush(self::Hand)
    rankList = getRankList(self)
    suit = self.sorted[1].suit

    for i = 1:5
        self.sorted[i].suit != suit && return false
    end 

    return self.sorted[5].suit == suit
end


# Determines if the hand is a straight.
function isStraight(self::Hand)
    rankList = getRankList(self)

    if (rankList[5] == 14 && rankList[1] == 2)
        rankList[5] = 1
        rankList = sort(rankList)
    end

    for i = 1:4
        (rankList[i + 1] != rankList[i] + 1) && return false
    end

    return true
end


# Determines if the hand is a three of a kind.
function isThreeOfAKind(self::Hand)
    rankList = getRankList(self)

    return (rankList[1] == rankList[3]) ||
           (rankList[2] == rankList[4]) ||
           (rankList[3] == rankList[5])
end


# Determines if the hand is a two pair.
function isTwoPair(self::Hand)
    rankList = getRankList(self)

    return ((rankList[1] == rankList[2]) && (rankList[3] == rankList[4])) ||
           ((rankList[1] == rankList[2]) && (rankList[4] == rankList[5])) ||
           ((rankList[2] == rankList[3]) && (rankList[4] == rankList[5]))
end


# Determines if the hand is a pair.
function isPair(self::Hand)
    rankList = getRankList(self)

    for i = 1:4
        (rankList[i] == rankList[i + 1]) && return true
    end

    return false
end


# Determines the tie breaking card of the hand depending on its handType.
function getTieBreakerCard(self::Hand, pass::Int)

    if (self.handType == 10) ; return self.sorted[4]

    elseif (self.handType == 9)
        return (self.sorted[4].rank == 14 && self.sorted[0].rank == 2) ?
                self.sorted[3] : self.sorted[4]

    elseif (self.handType == 8) ; return self.sorted[2]
    elseif (self.handType == 7) ; return self.sorted[2]
    elseif (self.handType == 6) ; return self.sorted[4]
    elseif (self.handType == 5) ; return self.sorted[4]
    elseif (self.handType == 4) ; return self.sorted[2]

    elseif (self.handType == 3)
        pairList = Card[]

        for i = 2:5
            currentCard = self.sorted[i]
            previousCard = self.sorted[i - 1]

            if (currentCard.rank - previousCard.rank == 0)
                push!(pairList, currentCard)
            end
        end

        max = pairList[0]
        min = pairList[0]

        for card in pairList
            if (card.rank > max.rank) ; max = card ; end
            if (card.rank < min.rank) ; min = card ; end
        end

        if     (pass == 0) ; return max
        elseif (pass == 1) ; return min
        else               ; return getKicker()
        end

    elseif (self.handType == 2)
        ret::Union{Card, Nothing} = nothing

        for i = 2:5
            currentCard = self.sorted[i]
            previousCard = self.sorted[i - 1]

            if (currentCard.rank - previousCard.rank) == 0
                ret = currentCard
                break
            end
        end

        if     (pass == 0) ; return ret
        elseif (pass == 1) ; return getKicker(self)
        end
    end

    return self.sorted[4]
end

# Helper method to get a sorted list of ranks in the hand.
function getRankList(self::Hand)
    rankList = Int[]

    for card in self.cards
        push!(rankList, card.rank)
    end

    return sort(rankList)
end


# Gets the kicker card for pairs and two pairs.
function getKicker(self::Hand)
    nonPairList = Card[]

    for i = 2:5
        currentCard = self.sorted[i]
        previousCard = self.sorted[i - 1]

        if (compare(currentCard, previousCard) != 0)
            push!(nonPairList, currentCard)
        end
    end

    max = nonPairList[1]

    for card in nonPairList
        if (compare(card, max) > 0) ; max = card ; end
    end

    return max;
end


# Returns a sorted version of a provided hand.
function sortHand(cardList::Vector{Card})
    for j in 1:4, i in 1:4
        if (compare(cardList[i], cardList[i + 1]) > 0)
            temp            = cardList[i + 1]
            cardList[i + 1] = cardList[i]
            cardList[i]     = temp
        end
    end

    return cardList
end