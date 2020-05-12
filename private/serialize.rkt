#lang racket/base


(require racket/generic racket/port racket/function json)

(provide
 jsexpr-serializable?
 gen:jsexpr-serializable
 prop:jsexpr-serializable
 jsexpr-serialize
 json-serialize
 current-jsexpr-deserializers
 (struct-out jsexpr-deserializer)
 jsexpr-deserialize
 json-deserialize)

(module+ test
  (require (prefix-in ru: rackunit)))

(define-values (prop:jsexpr-serializable jsexpr-serializable-type? jsexpr-serializer)
  (make-struct-type-property 'jsexpr-serializer))

(define-generics jsexpr-serializable
  (jsexpr-serialize jsexpr-serializable)  
  #:fast-defaults
  ([jsexpr? (define jsexpr-serialize identity)]
   [jsexpr-serializable-type?   
    (define (jsexpr-serialize jsexpr-serializable)
      ((jsexpr-serializer jsexpr-serializable) jsexpr-serializable))]))

(define (json-serialize input)
  (with-output-to-string
    (thunk (write-json (jsexpr-serialize input)))))

(module+ test
  (ru:check-true (jsexpr-serializable? #t))
  (ru:check-true (jsexpr-serializable? 1))
  (ru:check-true (jsexpr-serializable? "Hello, World!"))
  (ru:check-true (jsexpr-serializable? '(1 2 3 4)))
  (ru:check-true (jsexpr-serializable? (make-immutable-hash '((x . 1) (y . 2)))))

  (ru:check-false (jsexpr-serializable? 1+2i))
  (ru:check-false (jsexpr-serializable? #(1 2 3)))

  (struct point
    (x y) #:transparent
    #:property prop:jsexpr-serializable
    (λ (pt) (make-immutable-hash
	     `((point . (,(jsexpr-serialize (point-x pt))
			 ,(jsexpr-serialize (point-y pt))))))))

  (ru:check-true (jsexpr-serializable? (point 3 4)))
  (ru:check-equal? (jsexpr-serialize (point 3 4))
		   (make-immutable-hash '((point . (3 4)))))

  (struct person
    (first-name last-name)
    #:methods gen:jsexpr-serializable
    [(define/generic generic-jsexpr-serialize jsexpr-serialize)
     (define (jsexpr-serialize this)
       (make-immutable-hash
	`((person . ,(make-immutable-hash
		      `((first-name . ,(generic-jsexpr-serialize (person-first-name this)))
			(last-name  . ,(generic-jsexpr-serialize (person-last-name this)))))))))])

  (ru:check-equal? (jsexpr-serialize (person "John" "Smith"))
		   '#hash((person . #hash((first-name . "John") (last-name . "Smith"))))))

(struct jsexpr-deserializer
  (pattern converter))

(define current-jsexpr-deserializers
  (make-parameter '()))

(define (jsexpr-deserialize jsexpr)
  (let loop ([deserializers (current-jsexpr-deserializers)])
    (if (null? deserializers) jsexpr
      (if ((jsexpr-deserializer-pattern (car deserializers)) jsexpr)
	((jsexpr-deserializer-converter (car deserializers)) jsexpr)
	(loop (cdr deserializers))))))

(define (json-deserialize json-string)
  (call-with-input-string json-string
    (λ (inp) (jsexpr-deserialize (read-json inp)))))

(module+ test
  (require racket/match)
  (define point-deserizlizer
    (jsexpr-deserializer
     (match-lambda
       [(hash-table ('point (list x y))) #t]
       [_ #f])
     (match-lambda
       [(hash-table ('point (list x y)))
	(point x y)])))
  
  (let* ([pt (point 3 4)]
	 [point-jsexpr (jsexpr-serialize pt)]
	 [point-json (with-output-to-string (thunk (write-json point-jsexpr)))])
    (parameterize ([current-jsexpr-deserializers (cons point-deserizlizer (current-jsexpr-deserializers))])
      (ru:check-equal? (jsexpr-deserialize point-jsexpr) pt)
      (ru:check-equal? (json-deserialize point-json) pt))))

