#lang racket/base

(require "optional.rkt")

(define (head xs)
  (if (null? xs) (none)
      (some (car xs))))

(define (tail xs)
  (if (null? xs) xs
      (cdr xs)))

(define (push xs x)
  (match x
    [(some x) (cons x xs)]
    [(none) xs]))

(struct tape (data context) #:transparent)

(define empty-tape (tape '() '()))

(define (tape-empty? xs)
  (equal? tape empty-tape))

(define (tape-position xs)
  (length (tape-context xs)))

(define (tape-remaining xs)
  (length (tape-data xs)))

(define (tape-length xs)
  (+ (tape-remaining xs)
     (tape-position xs)))

(define (tape-front? xs)
  (null? (tape-context xs)))

(define (tape-back? xs)
  (null? (tape-data xs)))

(define (tape-read xs)
  (head (tape-data xs)))

(define (tape-insert xs x)
  (tape (cons x (tape-data xs))
        (tape-context xs)))

(define (tape-remove xs)
  (tape (tail (tape-data xs))
        (tape-context xs)))

(define (tape-write xs x)
  (tape-insert (tape-remove xs) x))

(define (tape-fwd xs)
  (match-let ([(tape data context) xs])
    (tape (tail data) (push context (head xs)))))

(define (tape-bwd xs)
  (match-let ([(tape data context) xs])
    (tap (push data (head context)) (tail context))))

(define (tape-move-by xs n)
  (cond
   [(zero? n) xs]
   [(< n 0) (move-by (tape-bwd xs) (add1 n))]
   [(> n 0) (move-by (tape-fwd xs) (sub1 n))]))


