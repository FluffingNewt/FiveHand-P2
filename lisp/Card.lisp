; Represents a playing card.
; This class defines a card object with a rank and suit.
; author: Davis Guest
(defclass Card ()
  ((rank :initarg :rank :accessor rank)
   (suit :initarg :suit :accessor suit)))


; Initializes a new card with the specified rank and suit.
(defmethod initialize-instance :after ((card Card) &key)
  (when (not (slot-boundp card 'rank))
    (setf (slot-value card 'rank) 0))
  (when (not (slot-boundp card 'suit))
    (setf (slot-value card 'suit) 0)))


; Returns a string representation of the card.
(defmethod to-string ((card Card))
  (let ((suitLabel (case (suit card) (0 "D")
                                     (1 "C")
                                     (2 "H")
                                     (3 "S")))

        (face      (case (rank card) (11 "J")
                                     (12 "Q")
                                     (13 "K")
                                     (14 "A")
                                     (t (write-to-string (rank card))))))

        (return-from to-string (concatenate 'string face suitLabel))))


; Compares this card with another card based on their ranks.
(defmethod compare ((card1 Card) (card2 Card))
  (- (rank card1) (rank card2)))