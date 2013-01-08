#lang racket/base
(require "primitives.rkt")

(provide empty-store
         store-ref
         store-set
         store-alloc)

(struct S (map))

(define empty-store (S (hasheq)))

(define (store-ref store address)
  (hash-ref (S-map store) address undefined*))

(define store-set
  (case-lambda
    [(store value)
     (let* ([address (gensym 'st)]
            [store (S (hash-set (S-map store) address value))])
       (values store address))]
    [(store address value)
     (S (hash-set (S-map store) address value))]))

(define (store-alloc store)
  (let ([address (gensym 'st)])
    (values (S (hash-set (S-map store) address undefined*)) address)))