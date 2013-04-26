#lang racket/base
(require racket/list
         racket/match)

(provide empty-stack
         stack-ref
         stack-set
         stack-set*
         stack-clear
         stack-push-uninit
         stack-push
         stack-pop
         stack-shift
         stack-reset)

(struct stack (slots height marks))

(define empty-stack (stack empty 0 empty))

(define (slots-set s u n)
  (when (empty? s)
    (error 'stack-set "stack ran out with ~a to go" n))
  (if (zero? n)
      (cons u (rest s))
      (cons (first s) (slots-set (rest s) u (sub1 n)))))

(define (slots-pop s n)
  (cond
    [(zero? n)
     s]
    [(empty? s)
     (error 'stack-pop "stack ran out with ~a to go" n)]
    [else
     (slots-pop (rest s) (sub1 n))]))

(define (slots-ref s n)
  (cond
    [(empty? s)
     (error 'slots-ref "stack ran out with ~a to go" n)]
    [(zero? n)
     (first s)]
    [else
     (slots-ref (rest s) (sub1 n))]))

(define (stack-ref s n)
  (slots-ref (stack-slots s) n))

(define (stack-set s u n)
  (stack (slots-set (stack-slots s) u n)))

(define (stack-set* s . u×n-s)
  (stack
   (foldl
    (λ (u×n s)
      (match-let ([(cons u n) u×n])
        (slots-set s u n)))
    (stack-slots s)
    u×n-s))
  (stack-height s)
  (stack-marks s))

(define (stack-clear s n)
  (stack-set s n 'uninit))

(define (stack-push-uninit s n)
  (define (slots-push-uninit s n)
    (if (zero? n)
        s
        (cons 'uninit
              (slots-push-uninit s (sub1 n)))))
  (stack (slots-push-uninit (stack-slots s) n)
         (+ (stack-height s) n)
         (stack-marks s)))

(define (stack-push s . us)
  (stack (append us (stack-slots s))
         (+ (stack-height s) (length us))
         (stack-marks s)))

(define (stack-pop s n)
  (stack (slots-pop (stack-slots s) n)
         (stack-height s)
         (stack-marks s)))

(define (stack-shift s)
  (let ([h (stack-height s)])
    (stack (stack-slots s)
           h
           (cons h (stack-marks s)))))

(define (stack-reset s)
  (match-let ([(stack s h (cons h′ ms)) s])
    (stack (slots-pop s (- h h′)) h′ ms)))
