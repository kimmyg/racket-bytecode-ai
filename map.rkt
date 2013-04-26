#lang racket/base
(require (except-in racket/list empty)
         racket/match)

(provide map
         empty
         ref
         set
         concat
         init
         extract)

(struct map (map))

(define empty (map (hasheq)))

(define (concat m0 . ms)
  (define (concat2 m0 m1)
    (define (hash-concat h0 h1)
      (for/fold ([h h0])
        ([(addr value) h1])
        (if (hash-has-key? h addr)
            (if (equal? (hash-ref h addr) value)
                h
                (error 'concat "values for ~a were not the same (~a and ~a)\n" addr (hash-ref h addr) value))
            (hash-set h addr value))))
    (map (map-map m0) (map-map m1)))
  (if (empty? ms)
      m0
      (let ([m1 (first ms)]
            [ms (rest ms)])
        (apply concat (concat2 m0 m1) ms))))

(define (ref m k)
  (hash-ref (map-map m) k))

(define (set m k v)
  (map (hash-set (map-map m) k v)))

(define (init ks vs)
  (map (for/fold ([h (hasheq)])
         ([k ks]
          [v vs])
         (hash-set h k v))))

(define (unzip a×b-s)
  (if (empty? a×b-s)
      (values empty empty)
      (let-values ([(a×b) (first a×b-s)]
                   [(as bs) (unzip (rest a×b-s))])
        (values (cons (car a×b) as)
                (cons (cdr a×b) bs)))))

(define (extract m)
  (unzip (hash->list (map-map m))))
