#lang racket/base

(provide empty-store
         store-ref
         store-set
         store-set-code)

(define empty-store (hash))

(define (store-ref s a)
  (if (hash-has-key? s a)
      (hash-ref s a)
      (error 'store-ref "no object at address ~a" a)))

(define (store-set s v [addr (gensym 'addr)])
  (values (hash-set s addr v) addr))

(define (store-set-code s code)
  (let ([addr0 (string->symbol (format "code~a" (equal-hash-code code)))]
        [addr1 (string->symbol (format "code~a" (equal-secondary-hash-code code)))])
    (if (hash-has-key? s addr0)
        (if (equal? (hash-ref s addr0) code)
            (values s addr0)
            (if (hash-has-key? s addr1)
                (if (equal? (hash-ref s addr1) code)
                    (values s addr1)
                    (error 'store-code "hash collisions on code ~a" code))
                (values (hash-set s addr1 code) addr1)))
        (values (hash-set s addr0 code) addr0))))