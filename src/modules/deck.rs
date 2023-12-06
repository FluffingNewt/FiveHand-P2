use crate::modules::card::Card;
use rand::seq::SliceRandom;
use std::fs::File;
use std::io::{BufRead, BufReader};

// Representing a collection of a set of standard 52 playing cards, without a Joker.
// This class allows you to build a deck either randomly or from a file and provides
// methods to draw cards from the deck.
// author: Davis Guest
pub struct Deck {
    pub cards     : Vec<Card>,
    pub duplicate : Option<Card>,
    pub deck_type : i8,
}

impl Deck {

    // Consutructs a new Deck with an empty list of cards and a default:
    // - deck type of -1
    // - duplicate of None
    pub fn new() -> Self {
        return Deck { cards: Vec::new(), duplicate: None, deck_type: -1 };
    }

    
    // Returns a string representation of the deck.
    pub fn to_string(&self) -> String {
        let mut list = String::new();

        for i in 1..=self.cards.len() {
            if self.cards[i - 1].rank != 10 {
                list.push(' ');
            }

            list += &self.cards[i - 1].to_string();

            if i == self.cards.len()                                 { break;
            } else if (i == 1 || i % 13 != 0) && self.deck_type == 0 { list.push(',');
            } else if (i == 1 || i % 5  != 0) && self.deck_type == 1 { list.push(',');
            } else                                                   { list.push('\n'); }
        }

        return list;
    }


    // Builds a random deck based on a standard deck of 52 playing cards without jokers.
    // Shuffles the deck to randomize the card order.
    pub fn build_rand_deck(&mut self) {
        self.deck_type = 0;

        for suit in 0..=3 {
            for rank in 2..=14 {
                self.cards.push(Card::new(rank, suit));
            }
        }

        self.cards.shuffle(&mut rand::thread_rng());
    }


    // Builds a deck based on an input file.
    pub fn build_file_deck(&mut self, file_path: &str) -> Result<(), std::io::Error> {
        self.deck_type = 1;

        let f = File::open(file_path)?;
        let file = BufReader::new(f);

        for line in file.lines() {
            let line = line?;
            let line_list: Vec<&str> = line.split(',').collect();

            for s in line_list {
                let i = if s.chars().next().unwrap_or(' ') == ' ' { 1 } else { 0 };

                let rank = match s.chars().nth(i) {
                    Some('1') => 10,
                    Some('J') => 11,
                    Some('Q') => 12,
                    Some('K') => 13,
                    Some('A') => 14,
                    Some(c) => c.to_digit(10).unwrap_or_default() as i8,
                    None => 0,
                };

                let suit = match s.chars().nth(2) {
                    Some('D') => 0,
                    Some('C') => 1,
                    Some('H') => 2,
                    Some('S') => 3,
                    _ => 0,
                };

                if let Some(card) = self.cards.iter().find(|&card| card.rank == rank && card.suit == suit) {
                    self.duplicate = Some(card.clone());
                }

                for card in &self.cards {
                    if card.rank == rank && card.suit == suit {
                        self.duplicate = Some(card.clone());
                        break;
                    }
                }

                self.cards.push(Card::new(rank, suit));
            }
        }

        Ok(())
    }


    // Draws a card from the deck, removing the first card.
    pub fn draw_card(&mut self) -> Card {
        return self.cards.remove(0);
    }
}