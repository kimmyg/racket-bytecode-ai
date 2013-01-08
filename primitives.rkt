#lang racket/base

(provide undefined*
         void*)

(define undefined* ((λ () (define x x) x)))
(define void* (void))         