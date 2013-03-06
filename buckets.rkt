#lang racket/base
(require racket/list)

(provide buckets-ref
         buckets-set)

(define (buckets-ref b n)
  (cond
    [(empty? b) (error 'buckets-ref "slot out of bounds by ~a" n)]
    [(zero? n) (first b)]
    [else (buckets-ref (rest b) (sub1 n))]))

(define (buckets-set b n v)
  (cond
    [(empty? b) (error 'buckets-set "slot for ~a out of bounds by ~a" v n)]
    [(zero? n) (cons v (rest b))]
    [else (cons (first b) (buckets-set (rest b) (sub1 n) v))]))
