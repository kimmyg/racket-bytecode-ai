#lang racket/base
(require racket/class
         racket/draw
         "stack.rkt")

(provide render)

(define (render V S H T C)
  (let* ([bmp (make-object bitmap% 200 200 #t)]
         [ctx (new bitmap-dc% [bitmap bmp])])
    (render-stack S ctx)
    bmp))
