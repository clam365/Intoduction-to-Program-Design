;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lam-c-hw1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; CHRISTOPHER LAM + COLIN NGUYEN   (cllam cnguyen1)

;;QUESTION 1

(define-struct date (year month day))
;; date is (make-date (Natural Natural Natural))
;; interp. (make-date (Natural Natural Natural)) with
;;   - year as the year of the opening date
;;   - month as the month of the opening date
;;   - day as the day of the opening date

;;Signatures:
;; - make-date: Natural Natural Natural -> date

;Examples
(define SPIDERMANNWHDATE (make-date 2021 12 17))
(define SONICTHEHEDGEHOG2DATE (make-date 2022 4 8 ))
(define NOTIMETODIEDATE (make-date 2021 11 8))
(define RANDOMMOVIEDATE (make-date 2022 9 1))
(define JAWSDATE (make-date 1975 6 20))
(define RANDOMMOVIE2DATE (make-date 2022 10 2))
(define FAKESPONGEBOBDATE (make-date 2022 9 2))
(define BLAHBLAHDATE (make-date 2022 9 2))

(define-struct film (title genre rating running-time opening-date nominations))
;; film is (make-film(String String String Natural date Natural))
;; interp. (make-film title genre rating running-time opening-date nominations) as a film with
;;    - title is the title of the film
;;    - genre is the genre of the film
;;    - rating is the rating of the film with the following: G, PG, PG-13, R, NC-17, NR
;;    - running-time is how long the movie is in minutes
;;    - opening-date is when the movie was first shown using the date function
;;    - nominations is the amount of Oscar nominations the film received


;;QUESTION 2
;;Signatures
;; - make-film: String String String Natural date Natural -> film
;; - film-title: film -> String
;; - film-genre: film -> String
;; - film-rating: film -> String
;; - film-running-time -> Natural
;; - film-opening-date film -> date
;; - film-nominations -> Natural
;; - film?: STRING -> Boolean

;;Purpose: consumes the date (openingdayrelease) struct, title, genre, rating, runtime, and amt of nominations to PRODUCE a film struct

;Examples
(define SPIDERMANNWH (make-film "Spiderman No Way Home" "Action" "PG-13" 157 SPIDERMANNWHDATE 0))
(define SONICTHEHEDGEHOG2 (make-film "Sonic the Hedgehog 2" "Action" "PG-13" 122 SONICTHEHEDGEHOG2DATE 0))
(define NOTIMETODIE (make-film "No Time to Die" "Action" "PG-13" 163 NOTIMETODIEDATE 1))
(define RANDOMMOVIE (make-film "Random Movie" "Drama" "NC-17" 170 RANDOMMOVIEDATE 0))
(define JAWS (make-film "Jaws" "Horror" "PG" 130 JAWSDATE 4))
(define RANDOMMOVIE2 (make-film "Random Movie 2" "Mystery" "NR" 100 RANDOMMOVIE2DATE 2))
(define FAKESPONGEBOB (make-film "Fake Spongebob" "Drama" "PG" 90 FAKESPONGEBOBDATE 10))
(define BLAHBLAH (make-film "Blah Blah" "Romance" "NR" 170 BLAHBLAHDATE 0))

;----------------------------------------------------------------------------------------------------------------------------

;;QUESTION 3

;;Signature:
;;  - high-brow? : film -> Boolean
;;Purpose: Consume a film and produce a boolean #true if it meets the criteria of being a drama and if > 2.5 hours (150min) long, OR is nominated for an oscar and is either rated NC-17 or NR, otherwise false 
;Stub: (define (high-brow? film))

(check-expect (high-brow? NOTIMETODIE) #false)
(check-expect (high-brow? SONICTHEHEDGEHOG2) #false)
(check-expect (high-brow? RANDOMMOVIE) #true)
(check-expect (high-brow? RANDOMMOVIE2) #true)
(check-expect (high-brow? JAWS) #false)
(check-expect (high-brow? SPIDERMANNWH) #false)
(check-expect (high-brow? FAKESPONGEBOB) #false)
(check-expect (high-brow? BLAHBLAH) #false)

(define (high-brow? film)
   (cond [(and (string=? (film-genre film) "Drama") (> (film-running-time film) 150)) #true]
         [(and (> (film-nominations film) 0) (or (string=? (film-rating film) "NC-17") (string=? (film-rating film) "NR"))) #true]
         [else #false]))

;------------------------------------------------------------------------------------------------------------------

;;QUESTION 4
;;Signature:
;;  - total-nominations: film film -> Natural
;;Purpose: consume two films and take each of its amount of Oscar nominations to produce a sum of the amount of both nominations

(check-expect (total-nominations SPIDERMANNWH SONICTHEHEDGEHOG2) 0)
(check-expect (total-nominations NOTIMETODIE RANDOMMOVIE) 1)
(check-expect (total-nominations RANDOMMOVIE2 JAWS) 6)

(define (total-nominations film1 film2)
  (+ (film-nominations film1) (film-nominations film2)))

;--------------------------------------------------------------------------------------------------------------------

;;QUESTION 5
;;Signature:
;; - update-nominations: film Natural -> film
;;Purpose: Consume a film and a Natural to produce the same film except with the original nominations replaved with the given Natural

(check-expect (update-nominations SPIDERMANNWH 3) (make-film "Spiderman No Way Home" "Action" "PG-13" 157 SPIDERMANNWHDATE 3)) ;test adding
(check-expect (update-nominations FAKESPONGEBOB 6) (make-film "Fake Spongebob" "Drama" "PG" 90 FAKESPONGEBOBDATE 6))  ;test removing
(check-expect (update-nominations BLAHBLAH 0) (make-film "Blah Blah" "Romance" "NR" 170 BLAHBLAHDATE 0)) ;test keeping the same

(define (update-nominations film newnom)
  (make-film (film-title film) (film-genre film) (film-rating film) (film-running-time film) (film-opening-date film) newnom)) ;;newnom is the new input

;---------------------------------------------------------------------------------------------------------------------

;;QUESTION 6
;;Signature:
;; - opened-after?: film date -> Boolean
;; - accessyear: film -> Natural
;; - accessmonth: film -> Natural
;; - accessday: film -> Natural
;;Purpose: consume a film and date and compares the film-opening-date to see if it opened after the given input date and produces a boolean


;;For helpful purposes, NOTIMETODIE date is (2021 11 8), SPIDERMANNWH is (2021 12 17)
(check-expect (opened-after? NOTIMETODIE (make-date 2021 11 8)) #false) ;checks where the date is the same as the film-opening-date
(check-expect (opened-after? NOTIMETODIE (make-date 2021 12 6)) #false) ;;checks the scenario where the year is the same of NOTIMETODIE as the given date's year but the opening month is after the film's date
(check-expect (opened-after? SPIDERMANNWH (make-date 2021 12 16)) #true) ;checks where the year and month are the same but the film opening DAY > given DAY
(check-expect (opened-after? NOTIMETODIE (make-date 2021 11 9)) #false) ;;checks the scenario where the year and month of NOTIMETODIE are the same as the opening date but the film's date day is before the opening-date's day
(check-expect (opened-after? NOTIMETODIE (make-date 2021 11 7)) #true) ;;checks the scenario where the year and month of NOTIMETODIE are the same as the opening date but the film's date is after the day of teh opening-date's day
(check-expect (opened-after? NOTIMETODIE (make-date 2020 4 6)) #true) ;; checks the scenario where the year of NOTIMETODIE is greater than the given date's year
(check-expect (opened-after? NOTIMETODIE (make-date 2042 5 15)) #false) ;; checks the scenario where NOTIMETODIE's film date year is less then the given date's year

;;THESE ARE OUR HELPER FUNCTIONS 
(define (accessyear film)
  (date-year (film-opening-date film))) ;;accesses the film year

(define (accessmonth film)
  (date-month (film-opening-date film)))  ;;accesses the film month

(define (accessday film)
  (date-day (film-opening-date film))) ;;accesses the film day


;; This program uses 3 conds in it with 3 else statements. It first evaluates the year comparison with 3 different possibilities. The comparison ends if it becomes false or true in that specific order. 
;; It will continue the comparisons when the years are equal to each other, leading to a new cond and repeating the process again with month and day.
;; If the input date is the same as the film-opening-date, then obviously it produces #false.
(define (opened-after? film date)
  (cond [ (< (accessyear film) (date-year date)) #false]
        [ (> (accessyear film) (date-year date)) #true] 
        [ else (cond [ (< (accessmonth film) (date-month date)) #false]  
                     [ (> (accessmonth film) (date-month date)) #true]
                     [ else (cond [ (< (accessday film) (date-day date)) #false]
                                  [ (> (accessday film) (date-day date)) #true]
                                  [ else #false])])]))
  
  
  

