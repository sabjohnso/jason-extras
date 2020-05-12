#lang racket/base

;; https://tools.ietf.org/html/rfc6901

(require json racket/match racket/string racket/port)

(module+ test
  (require rackunit))

(define json-path-string-pattern #px"/|/?(?:0|[1-9][0-9]*|[^/]+)(?:/(?:0|[1-9][0-9]*|[^/]+))*")

(define (json-path-element? x)
  (or (symbol? x) (exact-nonnegative-integer? x)))

(struct json-path (data absolute?))

(define (make-json-path-absolute path)
  (match-let ([(json-path data _) path])
    (json-path data #t)))

(define (json-path-string? str)
  (regexp-match? json-path-string-pattern str))

(define (json-path-string-absolute? str)
  (match str
    [(regexp #px"/.*" (list _)) #t]
    [(regexp #px".*" (list _)) #f]))

(define (string->key str)
  (call-with-input-string str
    (λ (inp) (read inp))))

(module+ test
  (check-equal? (string->key "0") 0)
  (check-equal? (string->key "dog") 'dog))

(define (string->json-path str)
  (let ([absolute? (json-path-string-absolute? str)])
    (json-path (map string->key (string-split str "/")) absolute?)))

(module+ test
  (check-true (json-path-string? "/"))
  (check-true (json-path-string? "0"))
  (check-true (json-path-string? "abcd10"))
  (check-true (json-path-string? "/3"))
  (check-true (json-path-string? "/abcd10"))
  (check-true (json-path-string? "/0/cat/dog/5"))

  (check-equal?  (json-path-data  (string->json-path "0/cat/dog/6"))
		 '(0 cat dog 6)))
