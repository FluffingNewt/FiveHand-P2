use crate::modules::deck::Deck;
use crate::modules::hand::Hand;

// Represents the game of Five Hand, a poker game with 6 hands.
// The game can be played with a randomized deck or a deck loaded from a file.
// author: Davis Guest
pub struct FiveHand {
    pub deck: Deck,
    pub hands: Vec<Hand>,
}

impl FiveHand {

    // Consutructs a new Five Hand game with a list of 6 empty hands and a deck of cards.
    pub fn new(file: String) -> Self {
        let mut deck = Deck::new();
        let mut hands: Vec<Hand> = Vec::new(); 

        for _i in 0..=5 {
            hands.push(Hand::new());
        }

        if !file.is_empty() { let _ = deck.build_file_deck(&file);
        } else { deck.build_rand_deck(); }

        return FiveHand {deck, hands};
    }


    // Starts a Five Hand game.
    pub fn play(&mut self, file: String) {
        println!("\n*** P O K E R   H A N D   A N A L Y Z E R ***\n");

        let game_type = if file.is_empty() { 0 } else { 1 };

        if game_type == 0 {
            println!(
                "\n*** USING RANDOMIZED DECK OF CARDS ***\n\
                \n*** Shuffled 52 card deck\n\
                {}",
                self.deck.to_string()
            );
        } else {
            println!(
                "\n*** USING TEST DECK ***\n\
                \n*** File: {}\n\
                {}",
                file,
                self.deck.to_string()
            );
        }

        if let Some(duplicate) = self.deck.duplicate {
            println!(
                "\n*** ERROR - DUPLICATED CARD FOUND IN DECK ***\n\
                \n*** DUPLICATE: {} ***\n",
                duplicate.to_string()
            );
            return;
        }

        self.draw_cards(game_type);

        println!("\n*** Here are the six hands...");

        self.print_all_hands();

        if game_type == 0 {
            println!(
                "\n*** Here is what remains in the deck...\n\
                {}",
                self.deck.to_string()
            );
        }

        println!("\n--- WINNING HAND ORDER ---");

        self.sort_hands();
        self.print_all_hands();
    }


    // Draws 30 cards to set up 6 hands of 5 cards.
    // Alternates drawing cards among the hands.
    pub fn draw_cards(&mut self, game_type: i8) {
        let mut hand_num = 0;
        if game_type == 0 {
            for _i in 0..30 {
                if hand_num == 6 { hand_num = 0; }
                self.hands[hand_num].add_card(self.deck.draw_card());
                hand_num += 1;
            }
        } else {
            for _i in 1..=30 {
                self.hands[hand_num].add_card(self.deck.draw_card());
                if _i % 5 == 0 { hand_num += 1; }
            }
        }
    }


    // Prints all the hands to the console.
    pub fn print_all_hands(&self) {
        for i in 0..6 {
            println!("{}", self.hands[i].to_string());
        }
    }


    // Sorts the hands to the winning order.
    pub fn sort_hands(&mut self) {
        for _j in 0..=4 {
            for i in 0..=4 {
                if self.hands[i].clone().compare(self.hands[i + 1].clone()) < 0 {
                    let temp = self.hands[i + 1].clone();
                    self.hands[i + 1] = self.hands[i].clone();
                    self.hands[i] = temp;
                }
            }
        }

        for i in 0..=5 {
            self.hands[i].assess_hand();
        }
    }
}
