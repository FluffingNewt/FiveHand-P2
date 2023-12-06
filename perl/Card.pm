# Represents a playing card.
# This class defines a card object with a rank and suit.
# author: Davis Guest
package Card;
use strict;
use warnings;


# Constructs a new card with the specified rank and suit.
sub new {
    my $class = shift;
    my $self  = bless {
         rank => shift,
         suit => shift,
    }, $class;

    return $self;
}


# Returns a string representation of the card.
sub toString {
    my $self = shift;

    my $suitLabel = "";
    if    ($self->{suit} == 0) { $suitLabel = "D"; }
    elsif ($self->{suit} == 1) { $suitLabel = "C"; }
    elsif ($self->{suit} == 2) { $suitLabel = "H"; }
    elsif ($self->{suit} == 3) { $suitLabel = "S"; }

    my $face = "";
    if    ($self->{rank} == 11) { $face = "J"; }
    elsif ($self->{rank} == 12) { $face = "Q"; }
    elsif ($self->{rank} == 13) { $face = "K"; }
    elsif ($self->{rank} == 14) { $face = "A"; }

    return ($face eq "") ? $self->{rank} . $suitLabel : $face . $suitLabel;
}


# Compares this card with another card based on their ranks.
sub compare {
    my $self  = shift;
    my $other = shift;
    return $self->{rank} - $other->{rank};
}

1