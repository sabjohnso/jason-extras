#lang racket/base

(require
 racket/contract json
 "private/serialize.rkt")

(provide
 gen:jsexpr-serializable
 prop:jsexpr-serializable
 (contract-out
  [jsexpr-serializable? predicate/c]
  [jsexpr-serialize (-> jsexpr-serializable? jsexpr?)]
  [json-serialize (-> jsexpr-serializable? string?)]  
  [struct jsexpr-deserializer ([pattern predicate/c] [converter (-> jsexpr? any/c)])]
  [current-jsexpr-deserializers (parameter/c (listof jsexpr-deserializer?))]
  [jsexpr-deserialize (-> jsexpr? any/c)]
  [json-deserialize (-> string? any/c)]))


