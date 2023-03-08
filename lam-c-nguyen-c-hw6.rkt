;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname lam-c-nguyen-c-hw6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
;Christopher Lam + Colin Nguyen (cllam +cnguyen1)

; QUESTION 1

(define-struct message (username text read?))
;; message is a (make-message String String Boolean)
;; interp. message as a message with
;;   - username as the username who sent the message
;;   - text as the body of the message
;;   - read? as whether the receiptient read the email

(define-struct user (username mailbox))
;; user is a (make-user String ListOfMessage)
;; interp. user as a user with
;;   - username as the username of the user
;;   - mailbox as a ListOfMessage of the user
 
;; a mailbox (ListOfMessage) is one of:
;; - empty, or
;; - (cons Message ListOfMessage)
;;
;; an email-system (ListOfUser) is one of:
;; - empty, or
;; - (cons User ListOfUser)

;----------------------------------------------------------------------------------
; QUESTION 2

(define mailsys empty)
; mailsys is an email system (ListOfUser) that is empty

(define newuser (make-user "Newuser" empty))
; newuser is a user with the username of "Newuser" and has an empty mailbox

;----------------------------------------------------------------------------------
; QUESTION 3

;; add-new-user: String -> (void)
;; consume a username and produce void, but in the background add a new user with the given username with an empty mailbox

(define (add-new-user username)
  (set! mailsys (append mailsys (list (make-user username empty))))) 

;Helper Function
;; add-new-user-helper: ListOfUser String -> Boolean
;; consume a mailsystem and a username, and searches through the list to see if the new user has been added to the list

(define (add-new-user-helper lou uname)
  (cond [ (empty? lou) #false]
        [ else
          (if (string=? uname (user-username (first lou)))
              #true
              (add-new-user-helper (rest lou) uname))]))

;CHECK EXPECTS
(add-new-user "yo")
(add-new-user "hello")
(add-new-user "knor")

(check-expect (add-new-user-helper mailsys "woiuvhwv") #false)
(check-expect (add-new-user-helper mailsys "yo") #true)
(check-expect (add-new-user-helper mailsys "knor") #true)
(check-expect (add-new-user-helper empty "yo") #false)
          
;----------------------------------------------------------------------------------
; QUESTION 4

;; send-email-message: String String String -> (void)/nothing
;; consume the sender name, receiptient name, and the message text
;; produce void
;; SIDE EFFECT: store a new unread message in the receipient's mailbox
       
(define (send-email-message sender-name recipient-name message-text)
  (insert-email sender-name recipient-name message-text mailsys))

;Helper Function
;; insert-email: String String String ListOfUser -> (void)/nothing
;; consume the sender name, receiptient name, message text, and an email system
;; produce nothing (void)
;; effect: store a new unread message in the receipient's mailbox
(define (insert-email sender-name recipient-name message-text a-lou)
  (cond [ (empty? a-lou) (error "user does not exist")]
        [ (cons? a-lou)
          (if (string=? recipient-name (user-username (first a-lou)))
              (set-user-mailbox! (first a-lou) (append (list (make-message sender-name message-text #false)) (user-mailbox (first a-lou))))
              (insert-email sender-name recipient-name message-text (rest a-lou)))])) 

;----------



;Helper Inside Check Expect Helper
;; searcher: ListOfMessage String String String -> Boolean
;; consume a ListOfMessage, sender name, recipient name, and message text
;; produce true if the unread message is in a user's mailbox, false otherwise
(define (searcher a-lom sender-name recipient-name message-t)
                         (cond [ (empty? a-lom) #false] 
                               [ else                               
                                 (if (and (string=? (message-username (first a-lom)) sender-name)
                                          (string=? (message-text (first a-lom)) message-t)
                                          (eq? #false (message-read? (first a-lom))))
                                     #true
                                     (searcher (rest a-lom) sender-name recipient-name message-t))]))

; ACTUAL Check Expect Helper
;; expectQ4: ListOfUser String String String -> Boolean
;; consume a ListOfUser, sender name, recipient name, a message
;; produce #true if the message is found within the user's mailbox, #false otherwise
;;    - in basic definitions, this used java concepts, with nested loops to iterate through the mailsystem with its users
;;      then if the user is found, iterate through its mailbox for the given message
;;    - uses double recursion

(define (expectQ4 a-lou sender-name recipient-name message-text)
  (cond [ (empty? a-lou) #false]
        [ else
          (if (string=? recipient-name (user-username (first a-lou)))
              (local [ (define backup (user-mailbox (first a-lou)))]
                       (searcher backup sender-name recipient-name message-text))
              (expectQ4 (rest a-lou) sender-name recipient-name message-text))]))


; Examples            
(send-email-message  "Chris Lam" "yo" "hi this is a test")
(send-email-message "blahblah" "yo" "false test")
(send-email-message  "Colin" "hello" "hi another test") 

(check-expect (expectQ4 mailsys "Chris Lam" "yo" "hi this is a test") #true) 
(check-expect (expectQ4 mailsys "blahblah" "yo" "false test") #true)
(check-expect (expectQ4 mailsys "nonexistent" "yo" "nonexistent") #false)
(check-expect (expectQ4 mailsys "wfwf" "notactuallyhere" "fakeness") #false)
(check-expect (expectQ4 mailsys "Colin" "hello" "hi another test") #true) 

;----------------------------------------------------------------------------------
; QUESTION 5

;; get-all-unread-messages: String -> ListOfMessage
;; consume a username
;; produce list of messages of that user with emails that are unread
;; EFFECT: make the unread emails read (#true)

;Examples
(set! mailsys (append mailsys (list (make-user "read emails" (list (make-message "wfwfwf" "sup" true))))))

(check-expect (get-all-unread-messages "yo") (list (make-message "blahblah" "false test" true) (make-message "Chris Lam" "hi this is a test" true)))
(check-expect (get-all-unread-messages "hello") (list (make-message "Colin" "hi another test" true)))
(check-expect (get-all-unread-messages "read emails") empty)
(check-expect (search-user "hello" mailsys) (list (make-message "Colin" "hi another test" true))) 
 
(define (get-all-unread-messages username)
  (search-user username mailsys))


;Helper Function
(define (search-user username a-lou)
  (cond [ (empty? a-lou) empty]
        [ else
          (if (string=? username (user-username (first a-lou)))
              (begin
                (local [ (define mailbox-list (user-mailbox (first a-lou)))
                         (define (search-unread mailbox-list)
                           (cond [ (empty? mailbox-list) empty]
                                 [ else
                                   (begin
                                     (if (eq? #false (message-read? (first mailbox-list)))
                                       (append (list (make-message (message-username (first mailbox-list)) (message-text (first mailbox-list)) #true)) (search-unread (rest mailbox-list)))
                                       (search-unread (rest mailbox-list))))]))]                  
                  (search-unread mailbox-list)))
              (search-user username (rest a-lou)))]))
                      
;----------------------------------------------------------------------------------
; QUESTION 6

;; most-total-messages: -> User
;; consume no parameters
;; produce the user with the most messages in their mailbox

;;EXAMPLES
(check-expect (most-total-messages) (make-user
  "yo"
  (list
   (make-message "blahblah" "false test" false)
   (make-message "Chris Lam" "hi this is a test" false))))

(check-expect (begin
                (send-email-message "sup" "hello" "wdwd")
                (send-email-message "wdw" "hello" "wdwdw")
                (send-email-message "wdfwdw" "hello" "wfwf")
                (most-total-messages))
              (make-user
 "hello"
 (list
  (make-message "wdfwdw" "wfwf" false)
  (make-message "wdw" "wdwdw" false)
  (make-message "sup" "wdwd" false)
  (make-message
   "Colin"
   "hi another test"
   false))))

  

(define (most-total-messages)
  (local
    [(define (find-most-total-messages a-lou acc)
       (cond [(empty? a-lou) acc]
             [(cons? a-lou) (if (> (length (user-mailbox (first a-lou))) (length (user-mailbox acc)))
                                   (find-most-total-messages (rest a-lou) (first a-lou))
                                   (find-most-total-messages (rest a-lou) acc))]))]
   (if (empty? mailsys)
       ("no users in the system")
       (find-most-total-messages (rest mailsys) (first mailsys)))))

 (most-total-messages)

;----------------------------------------------------------------------------------
; QUESTION 7

; (add-new-user "Mike")
; (add-new-user "Colin")
; (add-new-user "Pedro")

; (send-email-message "Mike" "Colin" "sup bro")
; (send-email-message "Colin" "Pedro" "hi dude")
; (send-email-message "Mike" "Pedro" "hi pedro")

; (get-all-unread-messages "Colin")
; (get-all-unread-messages "Pedro")
; (get-all-unread-messages "Mike")

; (most-total-messages)
 
;----------------------------------------------------------------------------------
; QUESTION 8

;; sum-of-string-lengths: ListOfString -> Natural
;; consume a ListOfString and produce a sum of the lengths of the string in the list

;Examples
(check-expect (sum-of-string-lengths (list "yo" "hi" "sup")) 7)
(check-expect (sum-of-string-lengths (list "mateo" "chris" "knor")) 14)
(check-expect (sum-of-string-lengths (list "" "" "")) 0) ;empty strings
(check-expect (sum-of-string-lengths (list "wpi" "goat" "gompei")) 13)
(check-expect (sum-of-string-lengths empty) 0) ;base case

(define (sum-of-string-lengths a-los)
  ;;acc: Number: the sum of each element in the list seen so far
  ;; (sum-of-string-lengths (list "yo" "hi" "sup"))
  ;; (sum-of-string-lengths (list "yo" "hi" "sup") 0)
  ;; (sum-of-string-lengths (list      "hi" "sup") 2)
  ;; (sum-of-string-lengths (list           "sup") 4)
  ;; (sum-of-string-lengths (list                ) 7)
  (local [ (define (sum-of-string-lengths a-los1 acc)
             (cond [ (empty? a-los1) acc]
                   [ else
                     (sum-of-string-lengths (rest a-los1) (+ acc (string-length (first a-los1))))]))]
    (sum-of-string-lengths a-los 0)))

;-----------------------------------------------------------------------------------
; QUESTION 9

;; one-long-string: ListOfString -> String
;; consume a ListOfString
;; produce a string with all the elements of ListOfString combined

;Examples
(check-expect (one-long-string (list "yo" "hello" "sup")) "yohellosup")
(check-expect (one-long-string  empty) "")
(check-expect (one-long-string (list "gompei")) "gompei")

(define (one-long-string a-los0)
  ;; acc: String; the strings combined from the a-los0 so far
  ;; (one-long-string (list "yo" "hello" "sup"))
  ;; (one-long-string (list "yo" "hello" "sup") "")
  ;; (one-long-string (list      "hello" "sup") "yo")
  ;; (one-long-string (list              "sup") "yohello")
  ;; (one-long-string (list                   ) "yohellosup")
  (local [ (define (one-long-string a-los acc)
             (cond [ (empty? a-los) acc]
                   [ else
                     (one-long-string (rest a-los) (string-append acc (first a-los)))]))]
    (one-long-string a-los0 "")))
                   