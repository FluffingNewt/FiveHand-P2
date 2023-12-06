(setf *random-state* (make-random-state t))

; Representing a collection of a set of standard 52 playing cards, without a Joker.
; This class allows you to build a deck either randomly or from a file and provides
; methods to draw cards from the deck.
; author: Davis Guest
(defclass Deck ()
  ((cards     :initarg :cards     :accessor cards)
   (duplicate :initarg :duplicate :accessor duplicate)
   (deckType  :initarg :deckType  :accessor deckType)))


; Initializes a new card with the specified rank and suit.
(defmethod initialize-instance :after ((deck Deck) &key)
    (setf (slot-value deck 'cards    ) (make-array 0 :element-type 'Card :adjustable t :fill-pointer 0))
    (setf (slot-value deck 'duplicate) nil)
    (setf (slot-value deck 'deckType ) -1))


; Returns a string representation of the card.
(defmethod to-string ((deck Deck))
  (let ((str ""))
    (loop for i from 1 to (length (cards deck))
      do (let ((card (elt (cards deck) (1- i))))
          (when (not (= (rank card) 10))
            (setf str (concatenate 'string str " ")))
          (setf str (concatenate 'string str (to-string card))))
        (cond ((= i (length (cards deck)))
                (return-from to-string str))
              ((and (or (= i 1) (not (= (mod i 13) 0))) (= (deckType deck) 0))
                (setf str (concatenate 'string str ",")))
              ((and (or (= i 1) (not (= (mod i 5) 0))) (= (deckType deck) 1))
                (setf str (concatenate 'string str ",")))
              (t
                (setf str (concatenate 'string str (format nil "~%"))))))))


; Adds an input card to the deck.
(defmethod add-card ((deck Deck) (card Card))
  (vector-push-extend card (cards deck)))


; Builds a random deck based on a standard deck of 52 playing cards without jokers.
(defmethod build-rand-deck ((deck Deck))
  (setf (deckType deck) 0)

  (dotimes (suit 4)
    (dotimes (rank 13)
        (add-card deck (make-instance 'Card :rank (+ rank 2) :suit suit))))

   (shuffle (cards deck)))


; Builds a deck based on an input file.
(defmethod build-file-deck ((deck Deck) (file string))
  (setf (deckType deck) 1)

  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line
          do (let ((line-list (comma-split line)))
                (loop for s in line-list
                      do  (let ((i (if (char= (char s 0) #\space) 1 0)))
                          (let ((rank (cond ((char= (char s i) #\1) 10)
                                            ((char= (char s i) #\J) 11)
                                            ((char= (char s i) #\Q) 12)
                                            ((char= (char s i) #\K) 13)
                                            ((char= (char s i) #\A) 14)
                                            (t (parse-integer (string (char s i)))))))
                            (let ((suit (cond ((char= (char s 2) #\D) 0)
                                              ((char= (char s 2) #\C) 1)
                                              ((char= (char s 2) #\H) 2)
                                              (t 3))))
                              (let ((duplicate (find-if (lambda (card)
                                                          (and (= (rank card) rank)
                                                              (= (suit card) suit)))
                                                        (cards deck))))
                                (when duplicate
                                  (setf (duplicate deck) duplicate)))
                              (add-card deck (make-instance 'Card :rank rank :suit suit))))))))
    (close stream)))


; Draws a card from the deck, removing the first card.
(defmethod draw-card ((deck Deck))
  (let ((card (aref (cards deck) 0)))
    (setf (cards deck) (subseq (cards deck) 1))
    (return-from draw-card card)))


; Splits an input string into a list by commas.
(defun comma-split (string)
  (loop for start = 0 then (1+ finish)
        for finish = (position #\, string :start start)
        collecting (subseq string start finish)
        until (null finish)))


; Shuffles the input sequence (i.e. list or vector).
(defun shuffle (v)
  (let ((n (length v)))
    (loop for i from 0 to ())
    (dotimes (i (1- n) v)
      (rotatef (aref v i) (aref v (+ i (random (- n i))))))))
