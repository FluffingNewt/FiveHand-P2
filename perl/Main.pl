#!/usr/bin/perl
use strict;
use warnings;
use Card;
use Deck;
use Hand;
use FiveHand;

my $filename = $ARGV[0];
my $game = FiveHand->new($filename);
$game->play($filename);