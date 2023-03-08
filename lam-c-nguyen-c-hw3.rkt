;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lam-c-nguyen-c-hw3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Christopher Lam + Colin Nguyen (cllam + cnguyen1)

;;QUESTION 1

;DATA DEFINITIONS
(define-struct Merchandise (name kind autographed? quantity price))
;; A Merchandise is a (make-Merchandise String String Boolean Natural Natural)
;; interp. (make-Merchandise name kind autographed? quantity price) as merchandise with:
;;  - name as the name of the merchandise
;;  - kind as the kind of merchandise (action figure, board game, costume, manga/comic book, trading card, etc.)
;;  - autographed? as a an indicator to whether the merchandise was autographed
;;  - quantity as the amount of that merchandise sold
;;  - price as the price of one unit

;Examples
(define tanjiro-funko-pop (make-Merchandise "Tanjiro Kamado Funko Pop" "action figure" #false 100 14))
(define monopoly (make-Merchandise "Monopoly" "board game" #false 20 10))
(define star-wars-comic (make-Merchandise "Star Wars Comic" "comic book" #true 15 10))
(define bicycle-cards (make-Merchandise "Bicycle Playing Cards" "playing card" #false 7 5))
(define pokemon (make-Merchandise "Pokemon Cards" "trading card" #false 100 10))
(define magicthegathering (make-Merchandise "Magic The Gathering Cards" "trading card" #false 200 10))

;---------------------------------------------------------------------------------------------
;QUESTION 2

;Templates
(define (merchandise-fcn merch)
  (... (Merchandise-name merch)
       (Merchandise-kind merch)
       (Merchandise-autographed? merch) 
       (Merchandise-quantity merch)
       (Merchandise-price merch)))
;----------------------------------------------------------------------------------------------
;QUESTION 3

;;DATA DEFINITIONS
;; A Receipt (ListOfMerchandise) is one of:
;;  -  empty (base case)
;;  -  (cons (Merchandise ListOfMerchandise))
;; interp. a ListOfMerchandise as a list of merchandise

;Examples
(define R1 (list monopoly bicycle-cards star-wars-comic))
(define R2 (list empty))
(define R3 (list tanjiro-funko-pop monopoly))
(define R4 (list tanjiro-funko-pop (make-Merchandise "rando" "action figure" #true 10 7)))

;-----------------------------------------------------------------------------------------------
;QUESTION 4

;Template
(define (fcn-for-lom lom)
  (cond [ (empty? lom) empty]
        [ (cons? lom) (... (first lom) (fcn-for-lom (rest lom)))]))

;-----------------------------------------------------------------------------------------------
;QUESTION 5

;Helper Functions
;;Signature: Merchandise Natural -> Boolean
;;Purpose: consume a merchandise and budget amount to see if it fits under the threshhold of whether its autographed and is under the budget line
(define (check-budget merch budget)
  (if (and (< (Merchandise-price merch) budget) (if (Merchandise-autographed? merch) 
                                                    #true
                                                    #false))
      #true
      #false))
      
;;Signature: ListOfMerchandise Natural -> ListOfMerchandise
;;Purpose: consume a ListOfMerchandise and a budget amount to produce a ListOfMerchandise with the new bounds of fitting the budget and is autographed
;Template
;(define (list-cheap-autograph lom budget)
;  (cond [ (empty? lom) empty]
;        [ (cons? lom) (... (first lom) (list-cheap-autograph (rest lom) budget))]))

;Examples
(check-expect (list-cheap-autograph R3 50) empty) ; case where nothing passes
(check-expect (list-cheap-autograph R1 40) (cons (make-Merchandise "Star Wars Comic" "comic book" #true 15 10) empty)) ;case where one passes through
(check-expect (list-cheap-autograph R1 8) empty) ;case where it is autographed but doesn't fit budget
(check-expect (list-cheap-autograph (list star-wars-comic (make-Merchandise "rando2" "blahblah" #true 100 200)) 20) (cons (make-Merchandise "Star Wars Comic" "comic book" #true 15 10) empty))
;test where both are autographed, but one doesn't fit the budget


(define (list-cheap-autograph lom budget)
  (cond [ (empty? lom) empty]
        [ (cons? lom) (if (check-budget (first lom) budget)
                          (cons (first lom) (list-cheap-autograph (rest lom) budget))
                          (list-cheap-autograph (rest lom) budget))]))

;---------------------------------------------------------------------------------------------------
;QUESTION 6

;Helper Function
;;Signature: Merchandise -> Boolean
;;Purpose: consume a Merchandise and produce a boolean to see whether if it's a trading card or not
(define (trading-card? merch)
  (if (string=? "trading card" (Merchandise-kind merch))
      #true
      #false))

;;Signature: ListOfMerchandise -> Natural
;;Purpose: consume a ListOfMerchandise and add up the quantity of merchandise that are "trading card"
;Template:
;(define (count-trading-cards lom)
;  (cond [ (empty? lom) empty]
;        [ (cons? lom) (... (first lom) (count-trading-cards (rest lom)))]))

;Examples
(check-expect (count-trading-cards empty) 0) ;case with an empty list (base case)
(check-expect (count-trading-cards (list pokemon magicthegathering)) 300) ;case with only trading cards
(check-expect (count-trading-cards R1) 0) ;case with no trading cards in the list
(check-expect (count-trading-cards (list monopoly tanjiro-funko-pop pokemon)) 100) ;case with both trading cards and others

 
(define (count-trading-cards lom)
  (cond [ (empty? lom) 0]
        [ (cons? lom) (if (trading-card? (first lom))
                          (+ (Merchandise-quantity (first lom)) (count-trading-cards (rest lom)))
                          (count-trading-cards (rest lom)))]))
 

;---------------------------------------------------------------------------------------------------
;QUESTION 7

;Helper Function
;;Signature: Merchandise -> Natural
;;Purpose: consume a Merchandise to produce the product of its price and quantity
(define (individual-cost merch)
  (* (Merchandise-quantity merch) (Merchandise-price merch)))

;;Signature: ListOfMerchandise -> Natural
;;Purpose: consume a ListOfMerchandise to produce the total cost (the total of each price x quantity of every Merchandise in the list added together)
;Template
;(define (receipt-total lom)
;  (cond [ (empty? lom) empty]
;        [ (cons? lom) (... (first lom) (receipt-total (rest lom)))]))

;Examples
(check-expect (receipt-total empty) 0) ;case with nothing in it
(check-expect (receipt-total (list pokemon magicthegathering)) 3000)
(check-expect (receipt-total R1) (+ (* 20 10) (* 7 5) (* 15 10))) ;case broken down


(define (receipt-total lom)
  (cond [ (empty? lom) 0]
        [ (cons? lom) (+ (individual-cost (first lom)) (receipt-total (rest lom)))]))

;---------------------------------------------------------------------------------------------------
;QUESTION 8

;Helper Function
;;Signature: Merchandise -> Boolean
;;Purpose: consume a Merchandise and determine whether its kind is a "board game" or not
(define (board-game? merch)
  (if (string=? "board game" (Merchandise-kind merch))
      #true
      #false))
;individual-cost also used


;;Signature: ListOfMerchandise -> Natural
;;Purpose: consume a ListofMerchandise and produce the total cost of all board games in the receipt (taking quantity into account)
;Template
;(define (board-games-cost lom)
;  (cond [ (empty? lom) empty]
;        [ (cons? lom) (... (first lom) (board-games-cost (rest lom)))]))

;Examples
(check-expect (board-games-cost empty) 0) ; empty
(check-expect (board-games-cost R1) 200) ; one board game
(check-expect (board-games-cost (list monopoly (make-Merchandise "rando3" "board game" #false 300 15))) (+ (* 20 10) (* 300 15)))
(check-expect (board-games-cost R4) 0) ;no board games

(define (board-games-cost lom)
  (cond [ (empty? lom) 0]
        [ (cons? lom) (if (board-game? (first lom))
                          (+ (individual-cost (first lom)) (board-games-cost (rest lom))) ;calls previous  helper function
                          (board-games-cost (rest lom)))]))

;----------------------------------------------------------------------------------------------------
;QUESTION 9

;had to fill in some examples
(define costume1 (make-Merchandise "princess costume" "costume" #false 10 40))
(define costume2 (make-Merchandise "pirate costume" "costume" #false 20 35))
(define costume3 (make-Merchandise "astronaut costume" "costume" #false 15 20))


;Helper Functions

;;Signature: Merchandise -> Boolean
;;Purpose: find out if the Merchandise kind is a costume
(define (costume? merch)
  (if (string=? "costume" (Merchandise-kind merch))
      #true
      #false))

;;Signature: Natural -> Natural
;;Purpose: consume the discount (in decimal form) and produce what actual % of the Merchandise cost would remain
(define (sale-application discount)
  (- 1 discount))


;Actual Function
;;Signature: ListOfMerchandise Natural -> Natural
;;Purpose: consume a ListOfMerchandise and a discount to produce the total cost with the discount applied to only costumes
;Template:
;(define (halloween-sale lom discount)
;  (cond [ (empty? lom) empty]
;        [ (cons? lom) (... (first lom) (halloween-sale (rest lom) discount))]))

;Examples
(check-expect (halloween-sale empty 0.25) 0) ;base case, nothing in it
(check-expect (halloween-sale R1 0.25) 385) ;no costume in it, no discount
(check-expect (halloween-sale (list costume1 costume2 pokemon) 0.25) (+ (* (- 1 0.25) 10 40) (* (- 1 0.25) 20 35) (* 100 10))) ;case with costumes and a regular
            
(define (halloween-sale lom discount)
  (cond [ (empty? lom) 0]
        [ (cons? lom) (if (costume? (first lom))
                          (+ (* (sale-application discount) (individual-cost (first lom))) (halloween-sale (rest lom) discount))
                          (+ (individual-cost (first lom)) (halloween-sale (rest lom) discount)))]))
  





































