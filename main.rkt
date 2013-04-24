#lang racket/base
(require "init.rkt"
         "load.rkt"
         "compile.rkt"
         "print.rkt"
         "primitive-maps.rkt")

(define p0 '(module test racket/base
              (add1 1)))

(define p1 '(module test racket/base
              (define (add1 x) (+ x 1))
              (add1 3)))

(define p2 '(module test racket/base
              (print "hello")))

((compose load init compile)
 p2)
