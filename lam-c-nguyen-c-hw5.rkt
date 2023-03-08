;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lam-c-nguyen-c-hw5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Christopher Lam + Colin Nguyen (cllam + cnguyen1)

;QUESTION 1

(define-struct river (name ph bloom? tributaries))
;; A river is (make-river String Natural Boolean ListOfRiver)
;;interp. (make-river name ph bloom? tributaries) as a river with:
;;  - name as the name of the river
;;  - ph as the pH of the water of the river
;;  - bloom? as a boolean of whether there is a presence of an algae bloom
;;  - tributaries as a ListOfRiver that feed into the river

;; A ListOfRiver is one of:
;;  - empty
;;  - (cons river ListOfRiver)

;------------------------------------------------------------------------------------------
;QUESTION 2

;Singular Rivers
(define YRAZ (make-river "Yusuf Raza River" 9.5 #true empty))
;(define MISSOURI (make-river "Missouri River" 5 #false empty))        ;base (top)
(define CHRIS (make-river "Chris River" 7 #true empty))
(define ALAIN (make-river "Alain River" 12 #false empty))
(define GRAND (make-river "Grand River" 5 #false (list ALAIN)))
(define MATEOSUF (make-river "Mateosuf River" 10 #true empty))
(define KNOR (make-river "Knor River" 6 #false (list CHRIS MATEOSUF)))

;River system
(define MISSOURI (make-river "Missouri River" 5 #false (list YRAZ KNOR GRAND)))

;------------------------------------------------------------------------------------------
;QUESTION 3

(define (fn-for-river a-river) 
  (... (river-name a-river)
       (river-ph a-river)
       (river-bloom? a-river) 
       (fn-for lor (river-tributaries a-river))))                                                          

(define (fn-for-lor lor)
  (cond [ (empty? lor) (...)] 
        [ else
          (... (fn-for river (first lor))
               (fn-for lor (rest lor)))])) 

;-------------------------------------------------------------------------------------------
;QUESTION 4

;; list-of-alkaline-rivers: river -> ListOfString
;; list-of-alkaline-rivers-helper: ListOfRiver -> ListOfString???
;; consume a river and produce a list of river names that have a pH of 9.5 or higher

;Examples
(check-expect (list-of-alkaline-rivers YRAZ) (list "Yusuf Raza River")) ;singular check
(check-expect (list-of-alkaline-rivers GRAND) (list "Alain River")) ;checking next node
(check-expect (list-of-alkaline-rivers KNOR) (list "Mateosuf River")) ;checking with 2 nodes
(check-expect (list-of-alkaline-rivers MISSOURI) (list "Yusuf Raza River" "Mateosuf River" "Alain River")) ;entire branches
(check-expect (list-of-alkaline-rivers-helper (list CHRIS YRAZ)) (list "Yusuf Raza River"))
(check-expect (list-of-alkaline-rivers-helper (list KNOR GRAND)) (list "Mateosuf River" "Alain River")) 
 
(define (list-of-alkaline-rivers a-river) 
  (if (>= (river-ph a-river) 9.5) 
      (cons (river-name a-river) (list-of-alkaline-rivers-helper (river-tributaries a-river)))
      (list-of-alkaline-rivers-helper (river-tributaries a-river)))) 

(define (list-of-alkaline-rivers-helper a-lor)
  (cond [ (empty? a-lor) empty]
        [ (cons? a-lor) (append (list-of-alkaline-rivers (first a-lor)) (list-of-alkaline-rivers-helper (rest a-lor)))]))

;-------------------------------------------------------------------------------------------
;QUESTION 5

;; algae-free?: river -> Boolean
;; algae-free-helper: -> ListOfRiver -> Boolean???
;; consume a river system and if the system has no algae bloom, produce true. false otherwise

;Examples
(check-expect (algae-free? MATEOSUF) #false) ;case with algae bloom
(check-expect (algae-free? ALAIN) #true) ;case with no algae bloom
(check-expect (algae-free? KNOR) #false) ;case with a node and 2 subnodes which contain algae blooms
(check-expect (algae-free? MISSOURI) #false) ;case with entire tree
(check-expect (algae-free-helper? (list CHRIS)) #false)
(check-expect (algae-free-helper? (list MISSOURI)) #false)
(check-expect (algae-free-helper? (list GRAND)) #true)

(define (algae-free? a-river)
  (if (not (river-bloom? a-river))
      (algae-free-helper? (river-tributaries a-river))
      #false))

(define (algae-free-helper? a-lor)
  (cond [ (empty?  a-lor) true]
        [ else
          (if (algae-free? (first a-lor))
              (algae-free-helper? (rest a-lor))
              #false)]))

;--------------------------------------------------------------------------------------------
; QUESTION 6

;; raise-all-ph: River -> River
;; raise-all-ph-helper: ListOfRiver -> ListOfRiver
;; consume a river system and produces a river system with all the rivers pH increased by 0.5

(check-expect (raise-all-ph YRAZ) (make-river "Yusuf Raza River" 10 #true empty))
(check-expect (raise-all-ph GRAND) (make-river "Grand River" 5.5 #false (list (make-river "Alain River" 12.5 #false empty))))
(check-expect (raise-all-ph-helper (list YRAZ)) (list (make-river "Yusuf Raza River" 10 #true empty)))

(define (raise-all-ph a-river)
   (make-river (river-name a-river) (+ 0.5 (river-ph a-river)) (river-bloom? a-river)
               (if (empty? (river-tributaries a-river))
                   (river-tributaries a-river)
                   (raise-all-ph-helper (river-tributaries a-river))))) 

(define (raise-all-ph-helper a-lor) 
  (cond [ (empty? a-lor) empty]
        [ else
          (cons (raise-all-ph (first a-lor)) (raise-all-ph-helper (rest a-lor)))]))

;--------------------------------------------------------------------------------------------
; QUESTION 7

;; find-subsystem: String River -> River or False
;; find-subsystem-helper: String ListOfRiver -> ListOfRiver or False
;; consume a river name and a river system and searches the system if it has that river
;;   - will then produce that river and its subroots, else false if it is not there

;Examples
(check-expect (find-subsystem "Grand River" ALAIN) #false) ;case where it't not there
(check-expect (find-subsystem "Grand River" GRAND) (make-river "Grand River" 5 #false (list ALAIN))) ;case where it's not there
(check-expect (find-subsystem "Knor River" MISSOURI) (make-river "Knor River" 6 #false (list CHRIS MATEOSUF))) ;case searching tree
(check-expect (find-subsystem-helper "Chris River" (list KNOR)) (make-river "Chris River" 7 #true empty))
(check-expect (find-subsystem-helper "Grand River" (list KNOR)) #false)

               
(define (find-subsystem name a-river) 
  (if (string=? name (river-name a-river))
      a-river
      (find-subsystem-helper name (river-tributaries a-river))))

(define (find-subsystem-helper name a-lor)
  (cond [ (empty? a-lor) #false]
        [ else
          (if (not (false? (find-subsystem name (first a-lor))))
              (find-subsystem name (first a-lor))
              (find-subsystem-helper name (rest a-lor)))]))

;--------------------------------------------------------------------------------------------
;; Part 2: Higher Order Functions

(define-struct Merchandise (name kind autographed? quantity price))
;; a Merchandise is a (make-merchandise String String Boolean Natural Number)
;; interp:
;; Merchandise represents an item sold at a pop culture emporium, where
;; name is the name of the merchandise item
;; kind indicates whether the item is an action figure, board game, costume,
;; manga/comic book, trading card, etc.
;; autographed? is true if the item is autographed
;; quantity is the number of that item that is being purchased
;; price is the cost of a single item

;; a Receipt (ListOfMerchandise) is one of
;; empty, or
;; (cons Merchandise Receipt)

(define tanjiro-funko-pop (make-Merchandise "Tanjiro Kamado Funko Pop" "action figure" #false 100 14))
(define monopoly (make-Merchandise "Monopoly" "board game" #false 20 10))
(define star-wars-comic (make-Merchandise "Star Wars Comic" "comic book" #true 15 8))
(define bicycle-cards (make-Merchandise "Bicycle Playing Cards" "playing card" #false 7 5))
(define pokemon (make-Merchandise "Pokemon Cards" "trading card" #false 100 10))
(define magicthegathering (make-Merchandise "Magic The Gathering Cards" "trading card" #false 200 10))

(define R1 (list monopoly bicycle-cards star-wars-comic))
(define R2 (list empty))
(define R3 (list tanjiro-funko-pop monopoly))
(define R4 (list tanjiro-funko-pop (make-Merchandise "rando" "action figure" #true 10 7)))

;--------------------------------------------------------------------------------------------
; QUESTION 8

;; bargain-items: ListOfMerchandise -> ListOfString
;; consumes a list of merchandise items and produces a list of the names of all the items with prices under $10

;Examples
(check-expect (bargain-items R3) empty) ;case with nothing fitting constraints
(check-expect (bargain-items R1) (list "Bicycle Playing Cards" "Star Wars Comic")) ;case where it fits budget
(check-expect (bargain-items R4) (list "rando"))
 
(define (bargain-items? a-lom)
  (local [(define (over-ten? a-merchandise)
            (< (Merchandise-price a-merchandise) 10))]
        (filter over-ten? a-lom)))

(define (bargain-items a-lom)
  (map Merchandise-name (bargain-items? a-lom)))

;--------------------------------------------------------------------------------------------
; QUESTION 9

;; *must* use a higher-order function
;; *must* use local for full credit

;; any-of-kind?: ListOfMerchandise String -> Boolean
;; consumes a ListOfMerchandise and a kind of merchandise item 
;; produces true if there is an item of that kind in the ListOfMerchandise

;Examples 
(check-expect (any-of-kind? R1 "not there") #false) ;case where it's not there
(check-expect (any-of-kind? R1 "board game") #true) ;case where that kind is there
(check-expect (any-of-kind? R4 "action figure") #true)
(check-expect (any-of-kind? R3 "comic book") #false)

(define (any-of-kind? a-lom string)
  (local [ (define (any-of-kind a-merchandise)
             (string=? string (Merchandise-kind a-merchandise)))]
    (ormap any-of-kind a-lom)))
           
;--------------------------------------------------------------------------------------------
; QUESTION 10

;; list-cheap-autograph: ListOfMerchandise Number -> ListOfMerchandise
;; consumes a list of merchandise items and returns a list of those
;; autographed items that cost at most the given amount

;Examples
(check-expect (list-cheap-autograph? R1 5) empty) ;does not fit constraints
(check-expect (list-cheap-autograph? R1 9) (list star-wars-comic)) ;fits constraints
(check-expect (list-cheap-autograph? (list (make-Merchandise "rando" "action figure" #true 10 7) star-wars-comic) 10) (list (make-Merchandise "rando" "action figure" #true 10 7) star-wars-comic)) 


(define (list-cheap-autograph? a-lom number)
  (local [ (define (list-cheap-autograph a-merchandise)
             (and (Merchandise-autographed? a-merchandise) (<= (Merchandise-price a-merchandise) number)))]
    (filter list-cheap-autograph a-lom))) 
           
