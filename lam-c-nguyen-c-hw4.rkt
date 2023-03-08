;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname lam-c-nguyen-c-hw4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Christopher Lam + Colin Nguyen (cllam + cnguyen1)

; QUESTION 1

(define-struct projectnode (project-id title advisor students left right))
;; a BST is one of
;;   false
;;   ProjectNode
;; a ProjectNode is a (make-projectnode Number String String ListOfStudent BST BST)
;; interp. false means no BST, or empty
;;  - project-id as the node key
;;  - title as the title of the project
;;  - advisor as the advisor of the project
;;  - students as the students (list) in the project
;;  - left and right are the left and right subtrees of the BST

;; INVARIANT: for a given projectnode:
;;  - project-id > all other project-id's in its left child
;;  - project-id < all other project-id's in its right child
;;  - same project-id never appears twice in the tree
 
(define-struct student (name email))
;; student is (make-student String String)
;; interp. student as a WPI student with
;;  - name as their name
;;  - email as their email address

;Examples
(define CHRIS (make-student "Chris Lam" "cllam@wpi.edu"))
(define GOMPEI (make-student "Gompei the Goat" "gompei@wpi.edu"))
(define COLIN (make-student "Colin Nguyen" "cnguyen1@wpi.edu"))
(define RANDO (make-student "rando" "rando@wpi.edu"))
(define JARED (make-student "Jared Sangil" "jsangil@wpi.edu"))

;;A ListOfStudent is one of:
;; - empty
;; - (cons student ListOfStudent)

;Examples
(define LOS1 (list RANDO))
(define LOS2 (list CHRIS GOMPEI))
(define LOS3 (list CHRIS GOMPEI COLIN RANDO))
;------------------------------------------------------
;QUESTION 2
(define BST1 (make-projectnode 17.2205 "Future Troll" "Dr.Racket" LOS2
                               (make-projectnode 15.8802 "Hudson In Morgan" "Dr.Pepper" LOS1 #false #false) #false))

(define BST2 (make-projectnode 24.3847 "Best Project Ever" "Dr. Doofenshmirtz" (list CHRIS COLIN RANDO) #false
                               (make-projectnode 27.2746 "Cool Project" "Dr. Sangil" (list JARED GOMPEI) #false #false)))

;big tree time
(define BST3 (make-projectnode 20.1836 "TopG Project" "Andrew Tate" (list JARED CHRIS COLIN RANDO GOMPEI) BST1 BST2))

;------------------------------------------------------- 
;;QUESTION 3
;;Template for BST
(define (fcn-for-bst t)
  (cond [ (false? t) (...)]
        [else
         (... (projectnode-project-id t)
              (projectnoce-title t)
              (projectnode-advisor t)
              (projectnoce-students t)
              (fcn-for-best (projectnode-left t)) 
              (fcn-for-best (projectnode-right t)))]))

  
;------------------------------------------------------
;QUESTION 4

;; BST Natural -> Boolean
;; consume a BST and a Department number, produce true if any of those projects in the BST are from that department

;Examples
(check-expect (any-in-dept? BST1 16) #false) ;not there
(check-expect (any-in-dept? BST1 17) #true) ;checks first node
(check-expect (any-in-dept? BST3 24) #true) ;checks most of bst
(check-expect (any-in-dept? BST3 21) #false)

;Helper Function
;; BST -> Natural
;; consume a BST and round the project id number to its whole number, it will not round up but always down
(define (rounder a-bst)
  (if (> (round (projectnode-project-id a-bst)) (projectnode-project-id a-bst))
      (- (round (projectnode-project-id a-bst)) 1) ;if it rounds up, must subtract one from the rounded
      (round (projectnode-project-id a-bst))))

(define (any-in-dept? a-bst dept)  
  (cond [ (false? a-bst) false]
        [ else
          (cond [(= dept (rounder a-bst)) true]
                [(< dept (rounder a-bst)) (any-in-dept? (projectnode-left a-bst) dept)]     
                [(> dept (rounder a-bst)) (any-in-dept? (projectnode-right a-bst) dept)])]))
           
;------------------------------------------------------
;QUESTION 5

;; BST Natural String -> BST 
;; consume a BST, a natural (project number), and a string representing an email
;;  - produced a BST, if a node contains the email, then it will remove it

;Examples
(check-expect (drop-student BST1 17 "notareal@wpi.edu") BST1) ;no alteration  
(check-expect (drop-student BST1 17 "cllam@wpi.edu") (make-projectnode 17.2205 "Future Troll" "Dr.Racket" (list GOMPEI) ;checks first node
                               (make-projectnode 15.8802 "Hudson In Morgan" "Dr.Pepper" LOS1 #false #false) #false))
(check-expect (drop-student BST2 27 "jsangil@wpi.edu") (make-projectnode 24.3847 "Best Project Ever" "Dr. Doofenshmirtz" (list CHRIS COLIN RANDO) #false
                               (make-projectnode 27.2746 "Cool Project" "Dr. Sangil" (list GOMPEI) #false #false))) ;checks node after the initial node
(check-expect (drop-student BST3 27 "jsangil@wpi.edu") (make-projectnode 20.1836 "TopG Project" "Andrew Tate" (list JARED CHRIS COLIN RANDO GOMPEI) BST1 (make-projectnode 24.3847 "Best Project Ever" "Dr. Doofenshmirtz" (list CHRIS COLIN RANDO) #false
                               (make-projectnode 27.2746 "Cool Project" "Dr. Sangil" (list GOMPEI) #false #false)))) ; checks entire bst

;Helper Function 1
;; String ListOfStudents -> Boolean
;; checks whether the email is in the list of students 
(define (find-email email los)
  (cond [ (empty? los) #false]
        [ else (if (string=? email (student-email (first los)))
                   #true
                   (find-email email (rest los)))]))   

;Helper Function 2
;;String ListOfStudents -> ListOfStudents
;;make a new list, if the original list contains the email, then remove it but keep all others
(define (make-new-los email los) 
  (cond [ (empty? los) empty]
        [ else 
          (if (find-email email los) 
              (make-new-los email (rest los)) 
              (cons (first los) (make-new-los email (rest los))))])) 
                             
(define (drop-student a-bst proj email) 
  (cond [ (false? a-bst) empty]
        [ else
          (cond [(< proj (rounder a-bst)) (make-projectnode (projectnode-project-id a-bst) (projectnode-title a-bst) (projectnode-advisor a-bst) (projectnode-students a-bst) (drop-student (projectnode-left a-bst) proj email) (projectnode-right a-bst))]     
                [(> proj (rounder a-bst)) (make-projectnode (projectnode-project-id a-bst) (projectnode-title a-bst) (projectnode-advisor a-bst) (projectnode-students a-bst) (projectnode-left a-bst) (drop-student (projectnode-right a-bst) proj email))] 
            [(= proj (rounder a-bst))
                 (if (find-email email (projectnode-students a-bst))
                     (make-projectnode (projectnode-project-id a-bst) (projectnode-title a-bst) (projectnode-advisor a-bst) (make-new-los email (projectnode-students a-bst)) (projectnode-left a-bst) (projectnode-right a-bst))
                     a-bst)])])) 

;---------------------------------------------------------------------------
;QUESTION 6 

;; BST -> ListOfTitles
;; consume a BST and produce a list of title in ascending order of its project number

;Examples
(check-expect (list-projects-in-order-by-id-num BST1) (list "Hudson In Morgan" "Future Troll"))
(check-expect (list-projects-in-order-by-id-num BST2) (list "Best Project Ever" "Cool Project"))
(check-expect (list-projects-in-order-by-id-num BST3) (list "Hudson In Morgan" "Future Troll" "TopG Project" "Best Project Ever" "Cool Project")) 
         
(define (list-projects-in-order-by-id-num a-bst)
  (cond [ (false? a-bst) empty]
        [ else
          (append (list-projects-in-order-by-id-num (projectnode-left a-bst))
                  (cons (projectnode-title a-bst)
                        (list-projects-in-order-by-id-num (projectnode-right a-bst))))])) 




;---------------------------------------------------------------------------
;QUESTION 7

;;BST Number String String -> BST 
;;consumes a Binary Search Tree, a project number, a project title, the name of the project advisor to
;;produce a Binary Search tree that is the same as the original execept with a new given project with the given information

;Examples
(check-expect (add-project #false 15.3245 "Project F" "Bobby") (make-projectnode 15.3245 "Project F" "Bobby" empty #false #false)) ;checks when there is no Binary Search Tree
(check-expect (add-project BST1 14.5454 "Project Z" "Ahrens") ;checks when the given NewProject is on the left of the Binary Search Tree
(make-projectnode 17.2205 "Future Troll" "Dr.Racket" (list (make-student "Chris Lam" "cllam@wpi.edu") (make-student "Gompei the Goat" "gompei@wpi.edu"))
(make-projectnode 15.8802 "Hudson In Morgan" "Dr.Pepper" (list (make-student "rando" "rando@wpi.edu"))
  (make-projectnode 14.5454 "Project Z" "Ahrens" empty #false #false) #false) #false))
(check-expect (add-project BST1 21.4323 "Project D" "Lucas") ;checks when the given NewProject is on the right of the Binary Search Tree
(make-projectnode 17.2205 "Future Troll" "Dr.Racket" (list (make-student "Chris Lam" "cllam@wpi.edu") (make-student "Gompei the Goat" "gompei@wpi.edu"))
(make-projectnode 15.8802 "Hudson In Morgan" "Dr.Pepper" (list (make-student "rando" "rando@wpi.edu")) #false #false)
(make-projectnode 21.4323 "Project D" "Lucas" empty #false #false)))

(define (add-project a-bst newproj newtitle newadvisor)
  (cond [(false? a-bst) (make-projectnode newproj newtitle newadvisor empty #false #false)]
        [else (if (< newproj (projectnode-project-id a-bst))
                           (make-projectnode
                                                   (projectnode-project-id a-bst)
                                                   (projectnode-title a-bst)
                                                   (projectnode-advisor a-bst)
                                                   (projectnode-students a-bst)
                                                   (add-project (projectnode-left a-bst) newproj newtitle newadvisor)
                                                   (projectnode-right a-bst))
                           (make-projectnode (projectnode-project-id a-bst)                                                                         
                                                   (projectnode-title a-bst)
                                                   (projectnode-advisor a-bst)
                                                   (projectnode-students a-bst)
                                                   (projectnode-left a-bst)
                                                   (add-project (projectnode-right a-bst) newproj newtitle newadvisor)))]))