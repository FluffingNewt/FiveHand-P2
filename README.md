# Project 2 - Five Card Stud - Second Five Languages

## Description
It's Five Card 2, *Electric Boogaloo*.  
Just like the first, this card game immitates a special version of poker called Five Card Stud and is written in 5 other languages this time around. This game does not allow for the splitting of the pot or ties. There is a clear winner each time depending on the hand's type, high card, and suit. There is no "draw phase" either, a randomized deck is looped through giving 1 card to one of six hands at a time (unless using a file input test deck).

## Game Types

There are two game types for this project, randomized (noraml) and file-input. When the program is ran with no command line arguements, the randomized game will be run as default. The file-input game uses a .txt listed in the command line arguements in order to test if a hand is accessed correctly. Hands will not be given cards one at a time, it will be line by line. Thus, this means the deck also is not randomized. The .txt file must have the following formatting:

* Space before all cards ranks/faces except for 10 (including the first card per line)
* Capital suit labels
* Comma after each suit except for the last card per line

Ex:  
 AH, 2H, 3H, 4H, 5H  
 2S, 3S, 4S, 5S, 6S  
 7S, 8S, 9S,10S, JS  
 QS, KS, 6H, 7H, 8H  
 9H,10H, JH, QH, KH  
 2C, 3C, 4C, 5C, 6C  

# Game Verions
In your terminal, navigate to where the game's files are located after downloading.

## Go
### Compile and Run
To compile and run all the .go files, type the following and press enter, make sure FiveHand.go is first: 

* go run FiveHand.go Card.go Deck.go Hand.go
* go run FiveHand.go Card.go Deck.go Hand.go {filename}.txt

## Julia
### Compile and Run
To compile and run all the .jl files, type the following and press enter: 

* julia FiveHand.jl
* julia FiveHand.jl {filename}.txt

## Lisp
### Compile and Run
To compile and run all the .lisp files, type the following and press enter: 

* sbcl --script FiveHand.lisp
* sbcl --script FiveHand.lisp {filename}.txt

## Perl
### Compile and Run
To compile and run all the perl files (main.pl and *.pm), type the following and press enter: 

* perl Main.pl
* perl Main.pl {filename}.txt

## Rust
### Compile and Run
To compile all the rust files, first cd into the "rust" directory. It is the one above the "scr" directory. Now type the following and press enter:

* cargo run rust
* cargo run rust {filename}.txt

## Author
Davis Guest  
CSC 330