#lang racket/base
(require compiler/zo-parse
         "init.rkt"
         "load.rkt"
         "compile.rkt"
         "step.rkt"
         "print.rkt")

(define p0 '(module test racket/base
              (add1 1)))

(define p1 '(module test racket/base
              (define (add1 x) (+ x 1))
              (add1 3)))

(define p2 '(module test racket/base
              (print "hello")))

(define (loop V S H T C n)
  (if (zero? n)
      (values V S H T C)
      (let-values ([(V S H T C) (step V S H T C)])
        (loop V S H T C (sub1 n)))))
  

#;(let-values ([(V S H T C) ((compose load init compile) p2)])
  (loop V S H T C 4))

(let-values ([(V S H T C) ((compose load init) (with-input-from-file "old/tests/t0_rkt_merged.zo" zo-parse))])
  (loop V S H T C 12))