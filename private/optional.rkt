#lang racket/base

(require
 racket/contract racket/class racket/match
 protocol)

(provide
 (contract-out
  [optional? predicate/c]
  [none (-> optional?)]
  [some (-> any/c optional?)])
 optional-monad)

(struct optional () #:transparent)
(struct some optional (value) #:transparent)
(struct none optional () #:transparent)

(define OptionalMonad%
  (class* ((compose derive-map/a derive-map/m)
           (send monad-fail instance-base))
    ((send monad-fail instance-interface))
    (super-new)
    (define/override (in-context? x) (optional? x))
    (define/override (map/f f mx)
      (match mx
        [(some x) (some (f x))]
        [_ (none)]))
    (define/override (return x) (some x))
    (define/override (join mmx)
      (match mmx
        [(some (some x)) (some x)]
        [_               (none)]))
    (define/override (fail) (none))))

(define optional-monad
  (new OptionalMonad%))


