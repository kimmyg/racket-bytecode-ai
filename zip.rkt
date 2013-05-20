#lang racket/base
(require racket/list)

(provide zip
         unzip)

(define (zip a b)
  (define (zipl a v)
    (if (empty? a)
        empty
        (cons (cons (first a) v) (zipl (rest a) v))))
  (define (zipr v b)
    (if (empty? b)
        empty
        (cons (cons v (first b)) (zipr v (rest b)))))
  (cond
    [(and (list? a) (list? b))
     (map cons a b)]
    [(and (list? a) (not (list? b)))
     (zipl a b)]
    [(and (not (list? a)) (list? b))
     (zipr a b)]
    [else
     (error 'zip "at least one argument must be a list; given ~a and ~a" a b)]))

(define (unzip a×b-s)
  (if (empty? a×b-s)
      (values empty empty)
      (let-values ([(a×b) (first a×b-s)]
                   [(as bs) (unzip (rest a×b-s))])
        (values (cons (car a×b) as)
                (cons (cdr a×b) bs)))))
