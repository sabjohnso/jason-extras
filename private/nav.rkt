#lang racket/base

(require
 racket/match racket/set racket/list json)

(module+ test
  (require rackunit))

(define object-pointer-keys (set 'key 'object))

(define tainted-index 0)
(define key-index 1)
(define object-index 2)
(define (object-pointer? x)
  (and (jsexpr? x)
       (list? x)
       (= (length x) 3)
       (boolean? (list-ref x tainted-index))
       (string? (list-ref x key-index))
       (hash? (list-ref x object-index))
       (hash-has-key? (list-ref x object-index) (string->symbol (list-ref x key-index)))))

(module+ test
  (let ([data `(#f "x"  ,(make-immutable-hash '((x . 3) (y . 4))))])
    (check-true (jsexpr? data))
    (check-true (object-pointer? data))))

(define index-index 1)
(define array-index 2)
(define array-pointer-keys (set 'index 'array))
(define (array-pointer? x)
  (and (jsexpr? x)
       (list? x)
       (= (length x) 3)
       (boolean? (list-ref x tainted-index))
       (exact-nonnegative-integer? (list-ref x index-index))
       (list? (list-ref x array-index))
       (< (list-ref x index-index ) (length (list-ref x array-index)))))

(module+ test
  (let ([data `(#f 0 ("a" "b" "c"))])
    (check-true (jsexpr? data))
    (check-true (array-pointer? data))))

(define (context? x)
  (and (jsexpr? x)
       (list? x)
       (for/fold ([accum #t])
           ([item x]
            #:break (not accum))
         (or (array-pointer? item)
             (object-pointer? item)))))

(module+ test
  (let ([data `((#f 0 ("a" "b" "c"))
                (#f "letters"
                    #hash((letters . ("a" "b" "c"))
                          (numbers . (1 2 3)))))])
    (check-true (jsexpr? data))
    (check-true (context? data))))

(define value-index 0)
(define context-index 1)

(define (navigation-data? x)
  (jsexpr? x)
  (list? x)
  (= (length x) 2)
  (context? (list-ref x context-index)))

(define (path-element? x)
  (or (string? x) (exact-nonnegative-integer? x)))

(define (path-data? x)
  (and (jsexpr? x)
       (list? x)
       (for/fold ([accum #f])
           ([item x]
            #:break (not accum))
         (path-element? item))))

;; (navigation-data? . -> . path-data?)
(define (position nav)
  (match nav
    [(list _ (and (? context?) context))
     (reverse (for/list ([item context])
                (car item)))]))

(define (open nav key/index)
  (match* (nav key/index)
    [((list (list xs ...) context) (and (? exact-nonnegative-integer?) index))
     (list (list-ref xs index)
           (cons (list #f index xs)
                 context))]
    [((list (and (? hash?) object) context) (and (? string?) key))
     (list (hash-ref object (string->symbol key))
           (cons (list #f key object)
                 context))]))

(module+ test
  (open '(("a" "b" "c") ()) 0)
  (close (open '(("a" "b" "c") ()) 0))

  )


(define (close nav)
  (match nav
    [(list _ '()) nav]
    [(list _  (list (list #f _ data) context ...)) (list data context)]

    [(list value (list (list #t (and (? exact-nonnegative-integer?) index) (list data ...+))))
     (list (list-set data index value) '())]

    [(list value (list (list #t (and (? exact-nonnegative-integer?) index) (list data ...+))
                       (list _ key/index more-data)
                       more-context ...))
     (list (list-set data index value)
           (cons (list #t key/index more-data)
                 more-context))]
    [(list value (list (list #t (and (? string?) key) (and (? hash?) data))))
     (list (hash-set data (string->symbol key) value) '())]
    
    [(list value (list (list #t (and (? string?) key) (and (? hash?) data))
                       (list _ key/index more-data)
                       more-context ...))
     (list (hash-set data (string->symbol key) value)
           (cons #t key/index more-data)
           more-context)]))
