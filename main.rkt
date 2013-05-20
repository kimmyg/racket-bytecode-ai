#lang racket/base
(require racket/list
         compiler/decompile
         compiler/zo-parse
         "init.rkt"
         "load.rkt"
         "compile.rkt"
         "step.rkt"
         "print.rkt")

(define p0 '(module test racket/base
              42))

(define p1 '(module test racket/base
              (values 42 1830)))

(define p5 '(module test racket/base
              (define (fact n)
                (if (zero? n)
                    1
                    (* n (fact (sub1 n)))))
              
              (fact 5)))

(decompile (compile+demodularize p5))

(define p12 '(module test racket/base
              (add1 1)))
(define p2 '(module test racket/base
              (define (add1 x) (+ x 1))
              (add1 3)))

(define p3 '(module test racket/base
              (print "hello")))

(define (interp V S H T C)
  (call-with-values (λ () (step V S H T C)) interp))
  
(define (interp-n V S H T C n)
  (if (zero? n)
      (values V S H T C)
      (let-values ([(V S H T C) (step V S H T C)])
        (interp-n V S H T C (sub1 n)))))

(define (interp-file path)
  (call-with-values
   (λ ()
     ((compose load init) (with-input-from-file path zo-parse)))
   interp))

(define (interp-n-file path n)
  (let-values ([(V S H T C) ((compose load init) (with-input-from-file path zo-parse))])
    (interp-n V S H T C n)))

#;(let-values ([(V S H T C) ((compose load init compile) p2)])
  (loop V S H T C 4))


;(interp-n-file "tests/t1_rkt_merged.zo" 53)

;(print (with-input-from-file "old/tests/t1_rkt_merged.zo" zo-parse))