# Represents a hand of playing cards.
# This class defines a hand object that can hold a collection of card objects.
# author: Davis Guest
package Hand;
use strict;
use warnings;
use List::Util qw(shuffle);


# Constructs a new hand with an empty list of cards, its sorted variant, and a default hand type of 0.
sub new {
    my $class = shift;
    my $self = bless {
        cards    => [],
        handType => 0,
    }, $class;

    return $self;
}


# Returns a string representation of the hand.
sub toString {
    my $self = shift;
    my $list = "";

    for my $i (1..$#{$self->{cards}} + 1) {
        if ($self->{cards}[$i - 1]->{rank} != 10) { $list .= " "; }

        $list .= $self->{cards}[$i - 1]->toString();

        if ($i == 0 || $i % 5 != 0) { $list .= " "; }
    }

    if ($self->{handType} == 0) { return $list };

    if    ($self->{handType} == 10) { $list .= " - Royal Straight Flush"; }
    elsif ($self->{handType} == 9)  { $list .= " - Straight Flush"; }
    elsif ($self->{handType} == 8)  { $list .= " - Four of a Kind"; }
    elsif ($self->{handType} == 7)  { $list .= " - Full House"; }
    elsif ($self->{handType} == 6)  { $list .= " - Flush"; }
    elsif ($self->{handType} == 5)  { $list .= " - Straight"; }
    elsif ($self->{handType} == 4)  { $list .= " - Three of a Kind"; }
    elsif ($self->{handType} == 3)  { $list .= " - Two Pair"; }
    elsif ($self->{handType} == 2)  { $list .= " - Pair"; }
    else                            { $list .= " - High Card"; }

    return $list;
}


# Adds a card to the hand's list of cards.
# param: card - Card representing the input card
sub addCard {
    my ($self, $card) = @_;
    push $self->{cards}, $card;
}


# Compares this hand with another hand based on their ranks.
# Used to sort each hand type and tiebreakers accordingly.
# param: other - Hand representing the hand to be compared.
# return: int representing the which hand is greater or less than.
sub compare {
    my ($self, $other) = @_;

    $self->assessHand();
    $other->assessHand();

    my $typeComparison = $self->{handType} - $other->{handType};

    return $self->compareHelper($other, $typeComparison, 0);
}


# Recursive helper method for the compare_hand method.
# param: other - Hand representing the hands to compare to.
# param: diff - int representing the difference of the hands in the current iteration.
# param: pass - int representing the number for the current pass.
# return: int representing the difference between the two hands.
sub compareHelper {
    my ($self, $other, $diff, $pass) = @_;

    if ($diff != 0) {return $diff;}

    my $thisBreaker  = $self->getTieBreakerCard($pass);
    my $otherBreaker = $other->getTieBreakerCard($pass);

    my $rankDiff = $thisBreaker->{rank} - $otherBreaker->{rank};
    my $suitDiff = $thisBreaker->{suit} - $otherBreaker->{suit};

    if ($pass < 2 && ($self->{handType} == 2 || $self->{handType} == 3)) {
        if ($rankDiff == 0) {
            return compareHelper($other, $rankDiff, $pass + 1)
        }
        return $rankDiff;
    }

    if ($rankDiff) {return $suitDiff;}

    return $rankDiff;
}


# Analyzes the current collection of cards in the hand and determines its hand type.
# The sorted instance variable is updated to contain the cards sorted by rank.
# This method sets the handType instance variable to one of the predefined hand types:
sub assessHand {
    my $self = shift;
    $self->sortHand();

    if    ($self->isRoyalStraightFlush()) { $self->{handType} = 10; }
    elsif ($self->isStraightFlush())      { $self->{handType} = 9; }
    elsif ($self->isFourOfAKind())        { $self->{handType} = 8; }
    elsif ($self->isFullHouse())          { $self->{handType} = 7; }
    elsif ($self->isFlush())              { $self->{handType} = 6; }
    elsif ($self->isStraight())           { $self->{handType} = 5; }
    elsif ($self->isThreeOfAKind())       { $self->{handType} = 4; }
    elsif ($self->isTwoPair())            { $self->{handType} = 3; }
    elsif ($self->isPair())               { $self->{handType} = 2; }
    else                                  { $self->{handType} = 1; }
}


# Determines if the hand is a royal straight flush.
sub isRoyalStraightFlush {
    my $self = shift;
    my @rankList = $self->getRankList();
    return $self->isStraightFlush() && $rankList[0] == 10 && $rankList[4] == 14;
}


# Determines if the hand is a straight flush.
sub isStraightFlush {
    my $self = shift;
    return $self->isStraight() && $self->isFlush();
}


# Determines if the hand is a four of a kind.
sub isFourOfAKind {
    my $self = shift;
    my @rankList = $self->getRankList();
    return $rankList[0] == $rankList[3] || $rankList[1] == $rankList[4];
}


# Determines if the hand is a full house.
sub isFullHouse {
    my $self = shift;
    my @rankList = $self->getRankList();
    return ($rankList[0] == $rankList[1] && $rankList[2] == $rankList[4]) ||
           ($rankList[0] == $rankList[2] && $rankList[3] == $rankList[4]);
}


# Determines if the hand is a flush.
sub isFlush {
    my $self = shift;
    my $suit = $self->{sorted}[0]->{suit};
    for my $i (0 .. 3) {
        if ($self->{sorted}[$i]->{suit} != $suit) {
            return 0;
        }
    }
    return $self->{sorted}[4]->{suit} == $suit;
}


# Determines if the hand is a straight.
sub isStraight {
    my $self = shift;
    my @rankList = $self->getRankList();
    if (grep { $_ == 14 } @rankList && grep { $_ == 2 } @rankList) {
        $rankList[0] = 1;
        @rankList = sort {$a <=> $b} @rankList;
    }
    for my $i (0 .. 3) {
        if ($rankList[$i + 1] != $rankList[$i] + 1) {
            return 0;
        }
    }
    return 1;
}


# Determines if the hand is a three of a kind.
sub isThreeOfAKind {
    my $self = shift;
    my @rankList = $self->getRankList();
    return $rankList[0] == $rankList[2] || $rankList[1] == $rankList[3] || $rankList[2] == $rankList[4];
}


# Determines if the hand is a two pair.
sub isTwoPair {
    my $self = shift;
    my @rankList = $self->getRankList();
    return ($rankList[0] == $rankList[1] && $rankList[2] == $rankList[3]) ||
           ($rankList[0] == $rankList[1] && $rankList[3] == $rankList[4]) ||
           ($rankList[1] == $rankList[2] && $rankList[3] == $rankList[4]);
}


# Determines if the hand is a pair.
sub isPair {
    my $self = shift;
    my @rankList = $self->getRankList();
    for my $i (0 .. 3) {
        if ($rankList[$i] == $rankList[$i + 1]) {
            return 1;
        }
    }
    return 0;
}


# Determines the tie breaking card of the hand depending on its handType.
sub getTieBreakerCard {
    my ($self, $pass) = @_;
    if ($self->{handType} == 10) { return $self->{sorted}[4]; }

    elsif ($self->{handType} == 9) {
        return ($self->{sorted}[4]->{rank} == 14 && $self->{sorted}[0]->{rank} == 2) ?
          $self->{sorted}[3] : $self->{sorted}[4];
    }
    elsif ($self->{handType} == 8) { return $self->{sorted}[2]; }
    elsif ($self->{handType} == 7) { return $self->{sorted}[2]; }
    elsif ($self->{handType} == 6) { return $self->{sorted}[4]; }
    elsif ($self->{handType} == 5) { return $self->{sorted}[4]; }

    elsif ($self->{handType} == 4) {
        my @pairList;
        for my $i (1 .. $#{$self->{sorted}}) {
            my $currentCard = $self->{sorted}[$i];
            my $previousCard = $self->{sorted}[$i - 1];
            if ($currentCard->{rank} == $previousCard->{rank}) {
                push @pairList, $currentCard;
            }
        }
        my $max = $pairList[0];
        my $min = $pairList[0];
        for my $card (@pairList) {
            if ($card->{rank} > $max->{rank}) {
                $max = $card;
            }
            if ($card->{rank} < $min->{rank}) {
                $min = $card;
            }
        }
        if ($pass == 0) {
            return $max;
        }
        elsif ($pass == 1) {
            return $min;
        }
        else {
            return $self->getKicker();
        }
    }

    elsif ($self->{handType} == 2) {
        my $ret = undef;
        for my $i (1 .. $#{$self->{sorted}}) {
            my $currentCard = $self->{sorted}[$i];
            my $previousCard = $self->{sorted}[$i - 1];
            if ($currentCard->{rank} == $previousCard->{rank}) {
                $ret = $currentCard;
                last;
            }
        }
        if ($pass == 0) {
            return $ret;
        }
        elsif ($pass == 1) {
            return $self->getKicker();
        }
    }
    return $self->{sorted}[4];
}


# Helper method to get a sorted list of ranks in the hand.
sub getRankList {
    my $self = shift;
    my @rankList;
    for my $card (@{$self->{cards}}) {
        push @rankList, $card->{rank};
    }
    @rankList = sort { $a <=> $b } @rankList;
    return @rankList;
}


# Gets the kicker card for pairs and two pairs.
sub getKicker {
    my $self = shift;
    my @nonPairList;
    for my $i (1 .. $#{$self->{sorted}}) {
        my $currentCard = $self->{sorted}[$i];
        my $previousCard = $self->{sorted}[$i - 1];
        if ($currentCard->{rank} != $previousCard->{rank}) {
            push @nonPairList, $currentCard;
        }
    }
    my $max = $nonPairList[0];
    for my $card (@nonPairList) {
        if ($card->{rank} > $max->{rank}) {
            $max = $card;
        }
    }
    return $max;
}


# Returns a sorted version of a provided hand.
sub sortHand {
    my $self = shift;

    for my $i (0..$#{$self->{cards}}) {
        $self->{sorted}[$i] = Card->new(
            $self->{cards}[$i]->{rank},
            $self->{cards}[$i]->{suit}
        );
    }

    for my $j (0 .. $#{$self->{sorted}} - 1) {
        for my $i (0 .. $#{$self->{sorted}} - 1) {
            if ($self->{sorted}[$i]->compare($self->{sorted}[$i + 1]) > 0) {
                my $temp = Card->new(
                    $self->{sorted}[$i + 1]->{rank},
                    $self->{sorted}[$i + 1]->{suit}
                );
                $self->{sorted}[$i + 1] = $self->{sorted}[$i];
                $self->{sorted}[$i] = $temp;
            }
        }
    }
}

1