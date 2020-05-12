#lang racket/base

(require "private/pretty.rkt")


(begin-for-syntax
 (define-syntax-class binding-pair
   (pattern (~seq name:id value:expr))))

(define-syntax (let-e stx)
  (syntax-parse stx
    [(_ )]))

