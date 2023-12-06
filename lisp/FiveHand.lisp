#!/usr/bin/sbcl --script
(load "Card.lisp")
(load "Deck.lisp")
(load "Hand.lisp")

(defvar f (if (= (length sb-ext:*posix-argv*) 2) (second sb-ext:*posix-argv*) ""))

; Represents the game of Five Hand, a poker game with 6 hands.
; The game can be played with a randomized deck or a deck loaded from a file.
; author: Davis Guest
(defclass FiveHand ()
  ((deck  :initarg :deck  :accessor deck )
   (hands :initarg :hands :accessor hands)))


; Initializes a new Five Hand game with a list of 6 empty hands and a deck of cards.
(defmethod initialize-instance :after ((fh FiveHand) &key)
  (let ((d (make-instance 'Deck))
        (h (make-array 0 :element-type 'Hand :adjustable t :fill-pointer 0)))
        
        (loop for i below 6 do
          (vector-push-extend (make-instance 'Hand) h))

        (if (not (equal f ""))
         (build-file-deck d f)
         (build-rand-deck d))

        (setf (slot-value fh 'deck) d)
        (setf (slot-value fh 'hands) h)))


; Starts a Five Hand game.
(defmethod play ((fh FiveHand))
  (format t "~%*** P O K E R   H A N D   A N A L Y Z E R ***~%")

  (setf game-type (if (string= f "") 0 1))

  (if (= game-type 0)
        (progn
          (format t "~%*** USING RANDOMIZED DECK OF CARDS ***~%")
          (format t "~%*** Shuffled 52 card deck ***~%")
          (format t "~a~%" (to-string (deck fh))))
        (progn
          (format t "~%*** USING TEST DECK ***~%")
          (format t "~%*** File: ~a ***~%" f)
          (format t "~a~%" (to-string (deck fh)))))

  (if (not (null (duplicate (deck fh))))
    (progn
      (format t "~%*** ERROR - DUPLICATED CARD FOUND IN DECK ***~%")
      (format t "~%*** DUPLICATE: ~a ***~%" (to-string (duplicate (deck fh))))
      (return-from play nil)))

  (draw-cards fh game-type)

  (format t "~%*** Here are the six hands...~%")
  (print-all-hands fh)

  (if (= game-type 0)
    (format t "~%*** Here is what remains in the deck...~%~a~%" (to-string (deck fh))))

  (format t "~%--- WINNING HAND ORDER ---~%")

  (sort-hands fh)
  (print-all-hands fh)
  (format t "~%"))


; Draws 30 cards to set up 6 hands of 5 cards.
(defmethod draw-cards ((fh FiveHand) (gt integer))
  (setf hand-num 0)
  (if (= gt 0)
        (loop for i from 0 below 30 do
              (if (= hand-num 6) (setf hand-num 0))
              (setf c (draw-card (deck fh)))
              (add-card (aref (hands fh) hand-num) c)
              (incf hand-num))
              
        (loop for i from 1 to 30 do
              (add-card (aref (hands fh) hand-num) (draw-card (deck fh)))
              (if (= (mod i 5) 0) (incf hand-num)))))


; Prints all the hands to the console.
(defmethod print-all-hands ((fh FiveHand))
  (loop for i below 6 do
    (format t "~a~%" (to-string (aref (hands fh) i)))))


; Sorts the hands to the winning order.
(defmethod sort-hands ((fh FiveHand))
  (loop for j from 0 to 4 do
    (loop for i from 0 to 4 do
      (if (< (compare-to (elt (hands fh) i) (elt (hands fh) (1+ i))) 0)
            (rotatef (elt (hands fh) i) (elt (hands fh) (1+ i)))))))


; Main Method Calls
(defvar game (make-instance 'FiveHand))
(play game)