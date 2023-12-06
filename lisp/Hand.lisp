; Represents a hand of playing cards.
; This class defines a hand object that can hold a collection of card objects.
; author: Davis Guest
(defclass Hand ()
    ((cards    :initarg :cards    :accessor cards   )
     (sorted   :initarg :sorted   :accessor sorted  )
     (handType :initarg :handType :accessor handType)))


; Initializes a new hand with an empty list of cards, its sorted variant, and a default hand type of 0.
(defmethod initialize-instance :after ((hand Hand) &key)
    (setf (slot-value hand 'cards) (make-array 0 :element-type 'Card :adjustable t :fill-pointer 0))
    (setf (slot-value hand 'sorted) (make-array 0 :element-type 'Card :adjustable t :fill-pointer 0))
    (setf (slot-value hand 'handType) 0))


; Adds a card to the hand's list of cards.
(defmethod add-card ((hand Hand) (card Card))
  (vector-push-extend card (cards hand)))


; Returns a string representation of the hand.
(defmethod to-string ((hand Hand))
  (let ((str ""))
    (loop for i from 1 to (length (cards hand)) do
          (if (/= (rank (aref (cards hand) (1- i))) 10)
              (setf str (concatenate 'string str " ")))

          (setf str (concatenate 'string str (to-string (aref (cards hand) (1- i)))))

          (if (/= (mod i 5) 0)
              (setf str (concatenate 'string str " "))))
    
    (cond ((= (handType hand) 0)  str)
          ((= (handType hand) 10) (concatenate 'string str " - Royal Straight Flush"))
          ((= (handType hand) 9)  (concatenate 'string str " - Straight Flush"))
          ((= (handType hand) 8)  (concatenate 'string str " - Four of a Kind"))
          ((= (handType hand) 7)  (concatenate 'string str " - Full House"))
          ((= (handType hand) 6)  (concatenate 'string str " - Flush"))
          ((= (handType hand) 5)  (concatenate 'string str " - Straight"))
          ((= (handType hand) 4)  (concatenate 'string str " - Three of a Kind"))
          ((= (handType hand) 3)  (concatenate 'string str " - Two Pair"))
          ((= (handType hand) 2)  (concatenate 'string str " - Pair"))
          (t                      (concatenate 'string str " - High Card")))))


; Compares this hand with another hand based on their ranks.
(defmethod compare-to ((hand Hand) (other Hand))
  (assess-hand hand)
  (assess-hand other)

  (let ((type-comparison (- (handType hand) (handType other))))
    (compare-to-helper hand other type-comparison 0)))


; Recursive helper method for the compare_hand method.
(defmethod compare-to-helper ((hand Hand) (other Hand) (diff integer) (pass integer))
  (if (/= diff 0)
      diff
      (let* ((this-breaker (get-tie-breaker-card hand pass))
             (other-breaker (get-tie-breaker-card other pass))
             (rank-diff (- (rank this-breaker) (rank other-breaker)))
             (suit-diff (- (suit this-breaker) (suit other-breaker))))
        (cond ((and (< pass 2) (or (= (handType hand) 2) (= (handType hand) 3)))
               (if (= rank-diff 0)
                   (compare-to-helper hand other rank-diff (1+ pass))
                   rank-diff))
              ((= rank-diff 0) suit-diff)
              (t rank-diff)))))


; Analyzes the current collection of cards in the hand and determines its hand type.
(defmethod assess-hand ((hand Hand))
  (sort-hand hand)

  (cond ((is-royal-straight-flush hand) (setf (handType hand) 10))
        ((is-straight-flush hand)       (setf (handType hand) 9))
        ((is-four-of-a-kind hand)       (setf (handType hand) 8))
        ((is-full-house hand)           (setf (handType hand) 7))
        ((is-flush hand)                (setf (handType hand) 6))
        ((is-straight hand)             (setf (handType hand) 5))
        ((is-three-of-a-kind hand)      (setf (handType hand) 4))
        ((is-two-pair hand)             (setf (handType hand) 3))
        ((is-pair hand)                 (setf (handType hand) 2))
        (t                              (setf (handType hand) 1))))


; Determines if the hand is a royal straight flush.
(defmethod is-royal-straight-flush ((hand Hand))
  (let ((rank-list (get-rank-list hand)))
    (and (is-straight-flush hand)
         (= (elt rank-list 0) 10)
         (= (elt rank-list 4) 14))))


; Determines if the hand is a straight flush.
(defmethod is-straight-flush ((hand Hand))
  (and (is-straight hand)
       (is-flush hand)))


; Determines if the hand is a four of a kind.
(defmethod is-four-of-a-kind ((hand Hand))
  (let ((rank-list (get-rank-list hand)))
    (or (= (elt rank-list 0) (elt rank-list 3))
        (= (elt rank-list 1) (elt rank-list 4)))))


; Determines if the hand is a full house.
(defmethod is-full-house ((hand Hand))
  (let ((rank-list (get-rank-list hand)))
    (or (and (= (elt rank-list 0) (elt rank-list 1))
             (= (elt rank-list 2) (elt rank-list 4)))
        (and (= (elt rank-list 0) (elt rank-list 2))
             (= (elt rank-list 3) (elt rank-list 4))))))


; Determines if the hand is a flush.
(defmethod is-flush ((hand Hand))
  (let ((suit (suit (elt (sorted hand) 0))))
    (loop for i below 4 do
          (unless (= (suit (elt (sorted hand) i)) suit)
            (return-from is-flush nil)))
    (= (suit (elt (sorted hand) 4)) suit)))


; Determines if the hand is a straight.
(defmethod is-straight ((hand Hand))
  (let ((rank-list (get-rank-list hand)))
    (if (and (= (elt rank-list 4) 14)
             (= (elt rank-list 0) 2))
        (progn (setf (elt rank-list 4) 1)
               (sort rank-list #'<))
        rank-list)
    (loop for i below 4 do
          (unless (= (elt rank-list (1+ i)) (+ (elt rank-list i) 1))
            (return-from is-straight nil)))
    t))


; Determines if the hand is a three of a kind.
(defmethod is-three-of-a-kind ((hand Hand))
  (let ((rank-list (get-rank-list hand)))
    (or (= (elt rank-list 0) (elt rank-list 2))
        (= (elt rank-list 1) (elt rank-list 3))
        (= (elt rank-list 2) (elt rank-list 4)))))


; Determines if the hand is a two pair.
(defmethod is-two-pair ((hand Hand))
  (let ((rank-list (get-rank-list hand)))
    (or (and (= (elt rank-list 0) (elt rank-list 1))
             (= (elt rank-list 2) (elt rank-list 3)))
        (and (= (elt rank-list 0) (elt rank-list 1))
             (= (elt rank-list 3) (elt rank-list 4)))
        (and (= (elt rank-list 1) (elt rank-list 2))
             (= (elt rank-list 3) (elt rank-list 4))))))


; Determines if the hand is a pair.
(defmethod is-pair ((hand Hand))
  (let ((rank-list (get-rank-list hand)))
    (loop for i below 4 do
          (when (= (elt rank-list i) (elt rank-list (1+ i)))
            (return-from is-pair t)))
    ))


; Determines the tie breaking card of the hand depending on its handType.
(defmethod get-tie-breaker-card ((h Hand) pass)
  (cond
    ((= (handType h) 10) (aref (sorted h) 4))
    ((= (handType h) 9)
     (if (and (= (rank (aref (sorted h) 4)) 14)
              (= (rank (aref (sorted h) 0)) 2))
         (aref (sorted h) 3)
         (aref (sorted h) 4)))
    ((= (handType h) 8) (aref (sorted h) 2))
    ((= (handType h) 7) (aref (sorted h) 2))
    ((= (handType h) 6) (aref (sorted h) 4))
    ((= (handType h) 5) (aref (sorted h) 4))
    ((= (handType h) 4) (aref (sorted h) 2))
    ((= (handType h) 3)
     (let* ((pair-list (make-array 0 :adjustable t :fill-pointer 0)))
       (loop for i from 1 below (length (sorted h)) do
             (let* ((current-card (aref (sorted h) i))
                    (previous-card (aref (sorted h) (1- i))))
               (when (= (rank current-card) (rank previous-card))
                 (vector-push-extend current-card pair-list))))

       (let* ((max (aref pair-list 0))
              (min (aref pair-list 0)))
         (loop for card across pair-list do
               (when (> (rank card) (rank max))
                 (setf max card))
               (when (< (rank card) (rank min))
                 (setf min card)))

         (cond
           ((= pass 0) max)
           ((= pass 1) min)
           (t (get-kicker h))))))
    ((= (handType h) 2)
     (loop for i from 1 below (length (sorted h)) do
             (let* ((current-card (aref (sorted h) i))
                    (previous-card (aref (sorted h) (1- i))))
               (when (= (rank current-card) (rank previous-card))
                 (return-from get-tie-breaker-card current-card))))

       (cond
         ((= pass 0) ret)
         ((= pass 1) (get-kicker h))))
         
      (t
        (aref (sorted h) 4))))


; Helper method to get a sorted list of ranks in the hand.
(defmethod get-rank-list ((hand Hand))
  (let ((rank-list (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for i below (length (cards hand)) do
          (vector-push-extend (rank (elt (cards hand) i)) rank-list))
    (sort rank-list #'<)
    rank-list))


; Gets the kicker card for pairs and two pairs.
(defmethod get-kicker ((hand Hand))
  (let ((non-pair-list (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for i from 1 below 5 do
          (let ((current-card  (elt (sorted hand) i))
                (previous-card (elt (sorted hand) (1- i))))
            (if (not (= (rank current-card) (rank previous-card)))
                (vector-push-extend current-card non-pair-list))))

    (let ((max (elt non-pair-list 0)))
      (loop for card across non-pair-list do
            (when (> (rank card) (rank max))
              (setf max card)))
      max)))


; Sets the sorted instance variable to a sorted version of a provided hand.
(defmethod sort-hand ((hand Hand))
    (loop for i below 5 do
        (vector-push-extend (make-instance 'Card :rank (rank (elt (cards hand) i))
                                                 :suit (suit (elt (cards hand) i)))
                            (sorted hand)))

    (loop for j below 4 do
        (loop for i below 4 do
            (if (> (compare (elt (sorted hand) i) (elt (sorted hand) (1+ i))) 0)
                (rotatef (elt (sorted hand) i) (elt (sorted hand) (1+ i)))))))