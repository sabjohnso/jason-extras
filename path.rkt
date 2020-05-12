#lang racket/base

(require "private/path.rkt")


(define current-json-path-delimiter (make-parameter "/"))
(define current-json-path-bactrack-delimiter (make-parameter ".."))
(define current-json-current-stationary-delimiter (make-parameter "."))

(struct back-track-type
  ())


(define (json-path-element? x)
  (or (exact-nonnegative-integer? x)
      (symbol? x)))

(define (json-path-element? x)
  (or (exact-nonnegative-integer? x)
      (symbol? x)))

(struct json-path
  (data absolute? delimiter)
  #:methods gen:custom-write
  ((define (write-proc this out mode)
     (map (λ (x) (format "~A~A" (json-path-delimiter this) x))
	  (cdr (json-path-data this))))))




(define (string->json-path str #:delimiter [delimiter "/"])
  (json-path
   (string-split str delimiter)
   (regexp-match? (pregexp (string-append "^" delimiter)) str)
   delimiter))


