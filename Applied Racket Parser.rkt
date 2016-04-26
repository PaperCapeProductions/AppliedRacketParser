#lang Racket
;Applied Racket Parser

;These first five definitions are useful 
;for all grammars.

(define inString empty)

;initializes the input string
(define (initString s)
    (set! inString s))

;returns the current symbol
(define (currentSymbol)
  (first inString))

;Before returning the current symbol, 
;resets the input string to the rest
;of the string so we're looking at the
;next symbol.
(define (moveToNextSymbol)
  (let ((x (first inString)))
    (begin
      (set! inString (rest inString))
    x)))

;Checks to make sure the current symbol
;is the expected one and if so, we go on to the next
;symbol.
(define (match expected)
    (cond
      ((null? inString) #f)
      ((eq? expected (currentSymbol))
       (begin 
         (moveToNextSymbol)
          #t))
      (else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Implements the rule S -> aSb
;Matches the a, checks to see if there should be another 
;call to S then, whether there is or not, matches the b.
; Note the AND.  The match of 'a AND the possible inside S AND
; the match of the 'b all have to be true.
(define (S1)
    (and
    (match 'a)
     (if (eq? (currentSymbol) 'a) (S1) #t)
     (match 'b)))

;Initializes the input string and returns true if
; the input string is empty, S -> e
;Otherwise, makes sure that S1 returns true AND we've read 
;the whole input string by the time we get back.
(define (isS1? in)
    (begin
      (initString in)
       (if (null? inString) #t (and (S1) (null? inString)))))

;Testing S1
(display "Testing S1 -> aS1b | e")
(newline)
(display "S1 -> (a a a b b b) should be true: ")
(isS1? '(a a a b b b))
(display "S1 -> (a a a b b) should be false: ")
(isS1? '(a a a b b))
(display "S1 -> () should be true: ")
(isS1? '())
(display "S1 -> (a a b b b) should be false: ")
(isS1? '(a a b b b))
;;;;;;;;;;;

 ; S2 -> aA2 | bB2
 ; A2 -> baA2 | e
 ; B2 -> abB2 | e
 
(define (A2)
   (if (null? inString) #t 
       (and (match 'b) (match 'a) (A2))))
 
(define (B2)
   (if (null? inString) #t 
       (and (match 'a) (match 'b) (B2))))

(define (S2)
    (cond
      ((null? inString) #f)
      ((eq? (currentSymbol) 'a)(and (match 'a) (A2)))
      (else (and (match 'b) (B2)))))

(define (isS2? in)
     (begin
       (initString in)
       (and (S2) (null? inString))))
;Testing S2

(newline)
(display "Testing S2 -> aA2 | bB2    A2 -> baA2 | e   B2 -> abB2 | e")
(newline)
(display "S2 -> (a b a b a) should be true: ")
(display "S2 -> (b a b a b) should be true: ")
(isS2? '(b a b a b))
(display "S2 -> () should be false: ")
(isS2? '())
(display "S2 -> (a) should be true: ")
(isS2? '(a))
(display "S2 -> (b a b b) should be false: ")
(isS2? '(b a b b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;3. For you to do:
; Grammar to implement and test:
; S3 -> aaS3 | B3
; B3 -> bB3ab | e
(define (B3)
  (cond
       ((null? inString) #t)
       (else
        (and
        (match 'b)
        (not (null? inString))
        (if (eq? (currentSymbol) 'b) (B3) #t)
        (match 'a)
        (match 'b)))))

(define (isB3? in)
     (begin
       (initString in)
       (and (B3) (null? inString))))


(define (S3)
  (cond
      ((null? inString) (B3))
      ((eq? (currentSymbol) 'a)
       (and (match 'a) (match 'a) (S3)))
      (else (B3))))

(define (isS3? in)
     (begin
       (initString in)
       (and (S3) (null? inString))))

(newline)
(display "Testing S3 -> aaS3 | B3")
(newline)
(display "S3 -> () should be true.")
(isS3? '() )
(display "S3 -> (a a) should be true.")
(isS3? '(a a) )
(display "S3 -> (a a b a b) should be true.")
(isS3? '(a a b a b) )
(display "S3 -> (a b a b a b) should be false.")
(isS3? '(a b a b a b) )

;4. For you to do:
; This is the language wcwR (That is, w is a word consisting of a's and b's, then there's a c,
; then we have the same word but reversed.)
; S4 -> aS4a | bS4b | c
(define (S4)
   (cond
      ((null? inString) #f)
      ((eq? (currentSymbol) 'a)
       (and (match 'a) (S4) (match 'a)))
      ((eq? (currentSymbol) 'b)
       (and (match 'b) (S4) (match 'b)))
      (else (match 'c))))
  

(define (isS4? in)
     (begin
       (initString in)
       (and (S4) (null? inString))))

(newline)
(display "Testing S4 -> aS4a | bS4b | c")
(newline)
(display "S4 -> () should be false.")
(isS4? '() )
(display "S4 -> (b b) should be false.")
(isS4? '(b b) )
(display "S4 -> (a a) should be false.")
(isS4? '(a a) )
(display "S4 -> (c) should be true.")
(isS4? '(c))
(display "S4 -> (a b c b a) should be true.")
(isS4? '(a b c b a))
(display "S4 -> (a a b b a c a b b a a) should be true.")
(isS4? '(a a b b a c a b b a a))
(display "S4 -> (a b a b) should be false.")
(isS4? '(a b a b))

;5. For you to do:
; This is the language of an odd number of a's and an even number of b's.
; S5 -> aD | bE
; D -> aS5 | bF | e
; E -> bS5 | aF
; F -> bD | aE

(define (S5)
    (cond
      ((null? inString) #f)
      ((eq? (currentSymbol) 'a)
       (and (match 'a) (D)))
      ((eq? (currentSymbol) 'b)
       (and (match 'b) (E)))))


(define (D)
     (cond
      ((null? inString) #t)
      ((eq? (currentSymbol) 'a)
       (and (match 'a) (S5)))
      ((eq? (currentSymbol) 'b)
       (and (match 'b) (F)))))

(define (E)
   (cond
      ((null? inString) #f)
      ((eq? (currentSymbol) 'a)
       (and (match 'a) (F)))
      ((eq? (currentSymbol) 'b)
       (and (match 'b) (S5)))))

(define (F)
   (cond
      ((null? inString) #f)
      ((eq? (currentSymbol) 'a)
       (and (match 'a) (E)))
      ((eq? (currentSymbol) 'b)
       (and (match 'b) (D)))))
     
(define (isS5? in)
     (begin
       (initString in)
       (and (S5) (null? inString))))

(newline)
(display "Testing S5 -> aD | bE")
(newline)
(display "S5 -> () should be false.")
(isS5? '() )
(display "S5 -> (a) should be true.")
(isS5? '(a) )
(display "S5 -> (a a a) should be true.")
(isS5? '(a a a) )
(display "S5 -> (a a a b b) should be true.")
(isS5? '(a a a b b) )
(display "S5 -> (b b) should be false.")
(isS5? '(b b) )
(display "S5 -> (a a a a a b b b b) should be true.")
(isS5? '(a a a a a b b b b) )
(display "S5 -> (a b b b b) should be true.")
(isS5? '(a b b b b) )
(display "S5 -> (a a b b b b) should be false.")
(isS5? '(a a b b b b) )