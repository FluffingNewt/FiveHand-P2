# Representing a collection of a set of standard 52 playing cards, without a Joker.
# This class allows you to build a deck either randomly or from a file and provides
# methods to draw cards from the deck.
# author: Davis Guest
package Deck;
use strict;
use warnings;
use List::Util qw(shuffle);


# Constructs a new Deck with an empty array of cards and a default:
# - deck type of -1
# - duplicate of undef
sub new {
    my $class = shift;
    my $self  = bless {
        cards     => [],
        duplicate => undef,
        deckType  => -1,
    }, $class;

    return $self;
}


# Returns a string representation of the deck.
sub toString {
    my $self = shift;

    my $list = "";
    for my $i (1..($#{$self->{cards}} + 1)) {
        if ($self->{cards}[$i - 1]->{rank} != 10) { $list .= " " }

        $list .= $self->{cards}[$i - 1]->toString();

        if    ($i == ($#{ $self->{cards} } + 1))                    { last; }
        elsif (($i == 1 || $i % 13 != 0) && $self->{deckType} == 0) { $list .= ","; }
        elsif (($i == 1 || $i % 5 != 0) && $self->{deckType} == 1)  { $list .= ","; }
        else                                                        { $list .= "\n"; }
    }

    return $list;
}


# Builds a random deck based on a standard deck of 52 playing cards without jokers.
# Shuffles the deck to randomize the card order.
sub buildRandDeck {
    my $self = shift;
    $self->{deckType} = 0;

    for my $s (0..3) {
        for my $r (2..14){
            push $self->{cards}, Card->new($r, $s);
        }
    }

    $self->{cards} = [shuffle @{$self->{cards}}];
}


# Builds a deck based on an input file.
# param: file - String representing the file to build the deck from.
sub buildFileDeck {
    my ($self, $file) = @_;
    $self->{deckType} = 1;

    open(DATA, "<$file") or die "Couldn't open file $file, $!";

    while (my $line = <DATA>) {
        chomp $line;
        my @lineList = split(',', $line);

        for my $str (@lineList) {
            my $i = (substr($str, 0, 1) eq ' ') ? 1 : 0;

            my $rank;
            if    (substr($str, $i, 1) eq '1') { $rank = 10; }
            elsif (substr($str, $i, 1) eq 'J') { $rank = 11; }
            elsif (substr($str, $i, 1) eq 'Q') { $rank = 12; }
            elsif (substr($str, $i, 1) eq 'K') { $rank = 13; }
            elsif (substr($str, $i, 1) eq 'A') { $rank = 14; }
            else                               { $rank = int(substr($str, $i, 1)); }

            my $suit;
            if    (substr($str, 2, 1) eq 'D') { $suit = 0; }
            elsif (substr($str, 2, 1) eq 'C') { $suit = 1; }
            elsif (substr($str, 2, 1) eq 'H') { $suit = 2; }
            else                              { $suit = 3; }

            my $duplicate;

            for my $card (@{ $self->{cards} }) {
                if ($card->{rank} == $rank && $card->{suit} == $suit) {
                    $duplicate = $card;
                }
            }

            push $self->{cards}, Card->new($rank, $suit);
        }
    }

    close DATA;
}


# Draws a card from the deck, removing the first card.
sub drawCard {
    my $self = shift;
    return shift $self->{cards};
}

1