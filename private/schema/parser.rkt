#lang racket/base

(struct parser () #:transparent)
(struct result parser (value) #:transparent)
(struct failure parser (message) #:transparent)
(struct item parser () #:transparent)
(struct empty (value) #:transparent)
(struct disj (p1 p2) #:transparent)
(struct conj (p1 p2) #:transparent)

(struct input
  )

(define (parse parser input)
  (match parser
    [(result value) `(#(,value ,jsexpr))]))

