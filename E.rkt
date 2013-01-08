#lang racket/base
(require racket/list
         "primitives.rkt")

(provide empty-env
         env-ref
         env-set
         env-clear
         env-push)

(struct E (stack))

(define empty-env (E empty))

(define (env-ref env n)
  (let ([stack (E-stack env)])
    (if (< n (length stack))
        (list-ref stack n)
        (error 'env-ref "out of bounds (~a in ~a)" n stack))))

(define (list-set xs n x)
  (if (zero? n)
      (cons x (rest xs))
      (cons (first xs) (list-set (rest xs) (sub1 n) x))))

(define (env-set env n v)
  (let ([stack (E-stack env)])
    (if (< n (length stack))
        (list-set stack n v)
        (error 'env-set "out of bounds (~a to ~a in ~a)" n v stack))))

(define (env-clear env n)
  (env-set env n undefined*))

(define (prepend x n xs)
  (if (zero? n)
      xs
      (cons x (prepend x (sub1 n) xs))))

(define (env-push env n [v undefined*])
  (E (prepend v n (E-stack env))))