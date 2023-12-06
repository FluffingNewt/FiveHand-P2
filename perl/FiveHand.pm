# Represents the game of Five Hand, a poker game with 6 hands.
# The game can be played with a randomized deck or a deck loaded from a file.
# author: Davis Guest
package FiveHand;
use strict;
use warnings;


# Constructs a new Five Hand game with a list of 6 empty hands and a deck of cards.
# If command line arguments are provided, it builds a file deck; otherwise, it builds a randomized deck.
sub new {
    my ($class, $file) = @_;
    my $self = bless {
        deck     => Deck->new(),
        hands    => [],
        gameType => undef,
    }, $class;

    for my $i (0..5) {
        push $self->{hands}, Hand->new();
    }

    if (defined $file) {
        $self->{gameType} = 1;
        $self->{deck}->buildFileDeck($file);
    } else {
        $self->{gameType} = 0;
        $self->{deck}->buildRandDeck();
    }

    return $self;
}


# Starts a Five Hand game.
sub play {
    my ($self, $file) = @_;

    print "\n*** P O K E R   H A N D   A N A L Y Z E R ***\n";

    if ($self->{gameType} == 0) { print "\n*** USING RANDOMIZED DECK OF CARDS ***\n" .
                                        "\n*** Shuffled 52 card deck\n" .
                                        $self->{deck}->toString() . "\n";

    } else { print "\n*** USING TEST DECK ***\n" .
                   "\n*** File: " . $file . "\n" .
                   $self->{deck}->toString() . "\n"; }

    if (defined $self->{deck}->{duplicate}) {
        print "\n*** ERROR - DUPLICATED CARD FOUND IN DECK ***\n" .
               "\n*** DUPLICATE: " . $self->{deck}->{duplicate}->toString() . " ***\n";
        return;
    }

    $self->drawCards($self->{gameType});

    print "\n*** Here are the six hands...\n";

    $self->printAllHands();

    if ($self->{gameType} == 0) {
        print "\n*** Here is what remains in the deck...\n" .
              $self->{deck}->toString() . "\n";
    }

    print "\n--- WINNING HAND ORDER ---\n";

    $self->sortHands();
    $self->printAllHands();
    print "\n";
}


# Draws 30 cards to set up 6 hands of 5 cards.
# Alternates drawing cards among the hands if the gametype is set to 0.
sub drawCards {
    my $self = shift;
    my $handNum = 0;
    if ($self->{gameType} == 0) {
        for my $i (0..29) {
            if ($handNum == 6) {$handNum = 0};
            $self->{hands}[$handNum]->addCard($self->{deck}->drawCard());
            $handNum++;
        }
    } else {
        for my $i (1..30) {
            $self->{hands}[$handNum]->addCard($self->{deck}->drawCard());
            if ($i % 5 == 0) {$handNum++};
        }
    }
}


# Sorts the hands to the winning order
sub sortHands {
    my $self = shift;

    for my $j (0..($#{$self->{hands}} - 1)) {
        for my $i (0..($#{$self->{hands}} - 1)) {
            if ($self->{hands}[$i]->compare($self->{hands}[$i + 1]) < 0) {
                my $temp               = $self->{hands}[$i + 1];
                $self->{hands}[$i + 1] = $self->{hands}[$i];
                $self->{hands}[$i]     = $temp;
            }
        }
    }
}

# Prints all the hands to the console.
sub printAllHands {
    my $self = shift;
    for my $i (0..5) {
        print $self->{hands}[$i]->toString() . "\n";
    }
}


1