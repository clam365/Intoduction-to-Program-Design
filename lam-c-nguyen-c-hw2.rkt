;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lam-c-nguyen-c-hw2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Christopher Lam + Colin Nguyen (cllam + cnguyen1)

;;DATA DEFINITIONS FOR WINDSTORM
;; windstorm is one of:
;;   - tornado
;;   - hurricane
;;   - thunderstorm
;;interp. windstorm as an itemizations (super-type) of our three windstorm compound data structures

;Template
(define (windstorm an-winds)
  (cond [ (tornado? an-winds) (... (tornado-scale an-winds)
                                   (tornado-distance an-winds)
                                   (tornado-max-winds an-winds))]
        [ (hurricane? an-winds) (... (hurricane-name an-winds)
                                     (hurricane-category an-winds)
                                     (hurricane-max-winds an-winds)
                                     (hurricane-velocity an-winds)
                                     (hurricane-heading an-winds))]
        [ (thunderstorm? an-winds) (... (thunderstorm-heading an-winds)
                                        (thunderstorm-velocity an-winds)
                                        (thunderstorm-rainfall an-winds)
                                        (thunderstorm-max-winds an-winds))]))
                                

(define-struct tornado (scale distance max-winds))
;;DATA DEFINITIONS
;;tornado is (make-tornado String Natural Natural)
;;interp. (make-tornado scale distance max-winds) as a tornado with
;;   - scale (based on Fujita scale rating)
;;   - distance which is the distance the tornado traveled in miles
;;   - max-winds as the maximum winds in miles per hour

;Examples of tornado
(define tornado1 (make-tornado "F5" 20 100))
(define tornado2 (make-tornado "F1" 10 60))
(define tornado3 (make-tornado "F3" 12 70))

;Template
(define (tornado-fcn a-tornado)
  (... (tornado-scale a-tornado)    
       (tornado-distance a-tornado)  
       (tornado-max-winds a-tornado)))


(define-struct hurricane (name category max-winds velocity heading))
;;DATA DEFINITIONS
;;hurricane is (make-hurricane (String Natural Natural Natural String))
;;interp. (make-hurricane (name category max-winds velocity heading)) as a hurricane with
;;   - name which is the name of the hurricane
;;   - category which is the category of the hurricae, between 1-5 inclusive
;;   - max-winds which is the maximum winds in miles per hour
;;   - velocity which is the velocity of the storm in miles per hour
;;   - heading which is the direction the hurricane is going towards

;Examples of hurricane
(define knor (make-hurricane "Knor" 5 220 200 "NW"))
(define agatha (make-hurricane "Agatha" 3 170 160 "SE"))
(define jared (make-hurricane "Jared" 2 100 80 "NE"))

;Template
(define (hurricane-fcn a-hurricane)
  (... (hurricane-name a-hurricane)    
       (hurricane-category a-hurricane)  
       (hurricane-max-winds a-hurricane)
       (hurricane-velocity a-hurricane)
       (hurricane-heading a-hurricane)))


(define-struct thunderstorm (heading velocity rainfall max-winds))
;;DATA DEFINITIONS
;;thunderstorm is (make-thunderstorm (String Natural Natural Natural))
;;interp. (make-thunderstorm (heading velocity rainfall max-winds)) as a thunderstorm with
;;   - heading which is the direction the thunderstorm is going
;;   - velocity which is the velocity of the thunderstorm in miles per hour
;;   - rainfall which the amount of rainfall from the thunderstorm in inches
;;   - max-winds as the maximum wind gust in miles per hour

;Examples of thunderstorm
(define thunderstorm1 (make-thunderstorm "NW" 40 6 51))
(define thunderstorm2 (make-thunderstorm "SW" 30 2 35))
(define thunderstorm3 (make-thunderstorm "E" 50 7 65))

;Template
(define (thunderstorm-fcn a-thunderstorm)
  (... (thunderstorm-heading a-thunderstorm)    
       (thunderstorm-velocity a-thunderstorm)  
       (thunderstorm-rainfall a-thunderstorm)
       (thunderstorm-max-winds a-thunderstorm)))

;---------------------------------------------------------------------------------------------
;;QUESTION 4
;;Signature: windstorm -> boolean
;;Purpose: consume a windstorm and check if the windstorm is
;;  - a tornado with a Fujita scale rating of "F4" or "F5"
;;  - a hurricane with a category of 4 or 5
;;  - OR a thunderstorm with >5in of rainfall and winds exceeding 50mph

;Template
;(define (violent? windstorm)
;  (cond [ (tornado? windstorm) ...]
;        [ (hurricane? windstorm) ...]
;        [ (thunderstorm? windstorm) ...]))

;this function will check whether the windstorm is one of these 3 sub-windstorm categories and do their respective evaluations
(define (violent? windstorm)
  (cond [ (tornado? windstorm) (if (or (string=? (tornado-scale windstorm) "F5") (string=? (tornado-scale windstorm) "F4"))
                                   #true
                                   #false)]
        [ (hurricane? windstorm) (if (or (= (hurricane-category windstorm) 4) (= (hurricane-category windstorm) 5))
                                     #true
                                     #false)]
        [ (thunderstorm? windstorm) (if (and (> (thunderstorm-rainfall windstorm) 5) (> (thunderstorm-max-winds windstorm) 50))
                                        #true
                                        #false)]
        [ else #false]))
 
                                    
;Examples
;tornado cases
(check-expect (violent? tornado1) #true)  ;true b/c scale of F5
(check-expect (violent? tornado2) #false) ; false b/c scale of F1
;hurricane cases
(check-expect (violent? knor) #true)   ; true b/c of cat5 
(check-expect (violent? agatha) #false) ; false b/c of cat3
;thunderstorm cases
(check-expect (violent? thunderstorm1) #true) ;true b/c the rainfall is over 5 inches and the windstorm is over 50mph
(check-expect (violent? (make-thunderstorm "E" 25 4 52)) #false) ;false but checking what happens when rainfall is less than 5 but the windstorm is still above 50
(check-expect (violent? (make-thunderstorm "N" 25 7 49)) #false) ;false but cheecking what happens when rainfall is above 5 but the windstorm is less than 50

;-------------------------------------------------------------------------------------------------
;QUESTION 5
;;Signature: windstorm Natural -> windstorm
;;Purpose: consume a an original windstorm and a natural representing wind-speeds to produce a new windstorm with the new change in its max-winds parameter

;Template
;(define (change-max-winds windstorm new-wind)
;  (cond [ (tornado? windstorm) (make-tornado ...)]
;        [ (hurricane? windstorm) (make-hurricane ...)]
;        [ (thunderstorm? windstorm) (make-thunderstorm ...)]))

;Examples
(check-expect (change-max-winds tornado1 120) (make-tornado (tornado-scale tornado1) (tornado-distance tornado1) 120)) ;increasing winds
(check-expect (change-max-winds jared 250) (make-hurricane (hurricane-name jared) (hurricane-category jared) 250 (hurricane-velocity jared) (hurricane-heading jared))) ;increasing winds
(check-expect (change-max-winds thunderstorm1 30) (make-thunderstorm (thunderstorm-heading thunderstorm1) (thunderstorm-velocity thunderstorm1) (thunderstorm-rainfall thunderstorm1) 30)) ;decreasing winds
(check-expect (change-max-winds thunderstorm1 51) (make-thunderstorm (thunderstorm-heading thunderstorm1) (thunderstorm-velocity thunderstorm1) (thunderstorm-rainfall thunderstorm1) 51)) ;same values

(define (change-max-winds windstorm newwind)
  (cond [(tornado? windstorm) (make-tornado (tornado-scale windstorm) (tornado-distance windstorm) newwind)]
        [(hurricane? windstorm) (make-hurricane (hurricane-name windstorm) (hurricane-category windstorm) newwind (hurricane-velocity windstorm) (hurricane-heading windstorm))]
        [(thunderstorm? windstorm) (make-thunderstorm (thunderstorm-heading windstorm) (thunderstorm-velocity windstorm) (thunderstorm-rainfall windstorm) newwind)]))

                              
;---------------------------------------------------------------------------------------------
;;QUESTION 6

;; a ListOfString is one of
;;  empty
;;  (cons String ListOfString)
;; interp:  ListOfString represents a list of strings

;;Signature: ListOfString -> String
;;Purpose: consume a ListOfStringt to produce a string with its first character of each string in the list

;Template
;(define (acrostic los)
;  (cond [ (empty? los) (...)]
;        [ (cons? los) (...(first los) (acrostic (rest los)))]))

;Examples
(check-expect (acrostic empty)"") ;; Checks when the list is empty
(check-expect (acrostic (list "hi" "who")) "hw") ;; Checks when the list has different starting letters
(check-expect (acrostic (list "aaa" "aaa" "aaaaaaa")) "aaa") ;; Checks when the lsit has the same starting letters

(define (acrostic los)
  (cond [ (empty? los) ""]
        [ (cons? los) (string-append (substring (first los) 0 1) (acrostic (rest los)))]))

;-------------------------------------------------------------------------------------------------------
;;QUESTION 7

;;Signature: ListOfString -> ListofString
;;Purpose: consume a ListOfString and return a new ListofString that has words with "ickle" in it
;Template
;(define (ickle-strings los)
;  (cond [ (empty? los) (...)]
;        [ (cons? los) (...(first los) (ickle-strings (rest los)))]))

;Examples
(check-expect (ickle-strings (list "pickle" "fickle" "hello")) (list "pickle" "fickle")) ;regular case finding the ickles
(check-expect (ickle-strings (list "wassup" "food" "h")) empty) ;case without any ickles
(check-expect (ickle-strings (list "entire" "line" "pIcKlE")) (list "pIcKlE")) ;case-insensitive
(check-expect (ickle-strings (list "PICKLES" "nickels" "tickle")) (list "PICKLES" "tickle")) ;also case-insensitive

;Helper function
(define (string-finder los)
  (string-contains-ci? "ickle" los)) ;;string-contains-ci? makes this case-insensitive


(define (ickle-strings los)
  (cond [ (empty? los) empty]
        [ (cons? los) (if (string-finder (first los))
                          (cons (first los) (ickle-strings (rest los)))
                          (ickle-strings (rest los)))]))

;------------------------------------------------------------------------------------------------------------
;QUESTION 8
                        
;;DATA DEFINITIONS
;; a ListOfNatural is one of
;;  - empty
;;  - (cons Natural ListofNatural)
;;interp. a ListofNatural as a list of numbers

;;Signature: ListofString -> ListofNatural
;;Purpose: consume a ListofString and produce a ListofNatural with each index having the # of characters from each index of ListofString
;Template
;(define (lengths-of-strings los)
;  (cond [ (empty? los) (...)]
;        [ (cons? los) (...(first los) (lengths-of-strings (rest los)))]))

(check-expect (lengths-of-strings empty) empty) ;empty base case
(check-expect (lengths-of-strings (list "hello" "hi" "yo")) (list 5 2 2)) ;case just counting
(check-expect (lengths-of-strings (list " ")) (list 1)) ;case with space
                          
(define (lengths-of-strings los)
  (cond [ (empty? los) empty]
        [ (cons? los) (cons (string-length (first los)) (lengths-of-strings (rest los)))]))
