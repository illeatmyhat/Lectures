  ;; To run this code: open a command prompt to the project directory, then type "lein run"

(ns blackjack.core
  (:gen-class))

(declare many-games
         dealer-strategy
         interactive-player-strategy greedy-player-strategy cautious-player-strategy inactive-player-strategy)

;;; Card, deck, and game constructors and selectors.
(defn make-card [suit kind]
  (list suit kind))

(defn card-suit [card]
  (first card))

(defn card-kind [card]
  (second card))
(defn new-deck []
  (for [suit (range 4)
        kind (range 1 14)]
    (make-card suit kind)))

(defn make-game [draw-pile player-hand dealer-hand]
  (list draw-pile player-hand dealer-hand))

(defn make-hand [card1 card2]
  (list card1 card2))

(defn add-to-hand [card hand]
  (cons card hand))

(defn draw-pile [game]
  (first game))

(defn player-hand [game]
  (second game))

(defn dealer-hand [game]
  (second (next game)))

(defn new-game []
  (let [deck (shuffle (new-deck))
        player (make-hand (first deck) (nth deck 2))
        dealer (make-hand (second deck) (nth deck 3))
        deck (drop 4 deck)]
    (make-game deck player dealer)))


;;; Utility functions.

;; Returns a string describing a card.
(defn card-string [card]
  (let [suits ["Spades" "Clubs" "Diamonds" "Hearts"]]
    (str (case (card-kind card)
           1 "Ace"
           11 "Jack"
           12 "Queen"
           13 "King"
           (str (card-kind card)))
         " of " (nth suits (card-suit card)))))

;; Gets the numeric value of a card.
(defn card-value [card]
  (let [kind (card-kind card)]
    (cond (= 1 kind) 11
          (<= 10 kind) 10
          :else kind)))

;; Gets the numeric value of a hand.
(defn hand-total [hand]
  ;; Find the simple sum of the card values in the hand, and also count the number of aces.
  (let [sum (reduce + 0 (map card-value hand))
        aces (count (filter #(= 1 (card-kind %)) hand))]
    ;; If the sum is less than 21, use that sum. If it is over 21, then reduce by 10 for each ace until the score
    ;; is under 21.
    (if (or (<= sum 22) (= 0 aces))
      sum
      (- sum (* 10 (min aces (int (Math/ceil (/ (- sum 21) 10)))))))))

;; Causes the player to "hit". Returns a new game state in which the specified player has an additional card
;; in their hand, with the draw-pile being reduced by the top card, and the other player's hand unchanged.
(defn hit [game-state which-player]
  ;; which-player will either be :player or :dealer
  (let [next-card (first (draw-pile game-state))
        new-player-hand (if (= :player which-player)
                          (add-to-hand next-card (player-hand game-state))
                          (player-hand game-state))
        new-dealer-hand (if (= :dealer which-player)
                          (add-to-hand next-card (dealer-hand game-state))
                          (dealer-hand game-state))
        new-draw-pile (next (draw-pile game-state))]
    (make-game new-draw-pile new-player-hand new-dealer-hand)))

(defn player-turn [game-state]
  (let [hand (player-hand game-state)
        score (hand-total hand)]
    (println "Player hand:" (clojure.string/join ", " (map card-string hand))
             ";" score "points")
    (if (< score 21)
      (let [next-state (interactive-player-strategy game-state)]
        (if (not= (count hand) (count (player-hand next-state)))
          (player-turn next-state)
          next-state))
      game-state)))

(defn dealer-turn [game-state]
  (let [hand (dealer-hand game-state)
        score (hand-total hand)]
  (println "Dealer's hand:" (clojure.string/join ", " (map card-string hand))
           ";" score "points")
  (let [next-state (dealer-strategy game-state)]
    (if (and (<= (hand-total (player-hand game-state)) 21)
             (not= (count hand) (count (dealer-hand next-state))))
      (do (println "Dealer hits")
          (dealer-turn next-state))
      next-state))))


;; Plays one full game of blackjack using the given new game state. Returns 0 if the game is a tie,
;; 1 if the player wins, -1 if the dealer wins.
(defn one-game [game-state]
  (println "Dealer is showing" (card-string (first (dealer-hand game-state))))
  (let [after-player (player-turn game-state)
        end-game (dealer-turn after-player)
        dealer-score (hand-total (dealer-hand end-game))
        player-score (hand-total (player-hand end-game))]
    ;; Player wins if player score is no larger than 21 AND (player score is greater than dealer, or dealer > 21).
    (if (and (<= player-score 21) (or (> player-score dealer-score) (> dealer-score 21)))
      1
      (if (= player-score dealer-score)
        0
        -1))))

(defn -main
  [& args]
  (let [winner (one-game (new-game))]
    (println (case winner
               1 "Player wins!"
               -1 "Dealer wins!"
               :else "Tie game")))
  )
  ;(let [totals (map vector (many-games 1000 0 0 0) ["player" "dealer" "tie"])]
    ;(println totals)))


;;; Automation

;; Runs n individual games. Returns a list of 3 values: the number of times the player won, the number of times
;; the dealer won, the number of ties.
(defn many-games
  ([n] (many-games n 0 0 0))
  ([n player-wins dealer-wins ties]
   (let [winner (one-game (new-game))]
     (if (= n 0)
       (list player-wins dealer-wins ties)
       (many-games
         (- n 1)
         (if (= winner 1) (+ player-wins 1) player-wins)
         (if (= winner -1) (+ dealer-wins 1) dealer-wins)
         (if (= winner 0) (+ ties 1) ties))))))


  ;;; Strategies.
;;; A strategy is a function that takes the current game state and returns a new game state representing a single
;;; decision made by the corresponding player.

;; Dealer strategies
;; The dealer does not get to make decisions: they always hit if less than 17, and never if at or above 17.
(defn dealer-strategy [game-state]
  (if (< (hand-total (dealer-hand game-state)) 17)
    (hit game-state :dealer)
    game-state))

;; Player strategies
;; The interactive strategy bases whether to hit or stay on input from the user, unless they are at
;; 21 or over.
(defn interactive-player-strategy [game-state]
    (if (< (hand-total (player-hand game-state)) 21)
      (do (println "(h)it or (s)tay?")
          (flush)
          (let [input (read-line)]
            (if (= "h" input)
              (hit game-state :player)
              game-state)))
      game-state))

;; The greedy player strategy always hits until they reach or exceed 21.
(defn greedy-player-strategy [game-state]
  (if (< (hand-total (:player-hand game-state)) 21)
    (do (println "Player hits")
        (hit game-state :player-hand))
    (do (println "Player stays")
        game-state)))

;; The cautious player strategy only hits when under 15.
(defn cautious-player-strategy [game-state]
  (if (< (hand-total (:player-hand game-state)) 15)
    (do (println "Player hits")
        (hit game-state :player-hand))
    (do (println "P-layer stays")
        game-state)))

;; The inactive player strategy never hits.
(defn inactive-player-strategy [game-state]
  (do (println "Player stays")
      game-state))