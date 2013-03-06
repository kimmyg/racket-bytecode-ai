#lang racket/base
(require racket/list)

(provide empty-env
         env-push
         env-push-n
         env-pop
         env-pop-n
         env-ref
         env-set
         env-clear)

; these marks are /not/ continuation marks
(struct env (stack marks))

(define (env-mark e [mark (gensym 'mark)])
  (let ([stack (env-stack e)]
        [marks (env-marks e)])
    (values (env stack (hash-set marks mark (length stack))) mark)))

(define (env-reset e mark)
  (define (discard xs n)
    (cond
      [(zero? n) xs]
      [(empty? xs) (error 'env-reset "bad use of mark ~a" mark)]
      [else (discard (rest xs) (sub1 n))]))
  (let ([stack (env-stack e)]
        [marks (env-marks e)])
    (env (discard stack (- (length stack) (hash-ref marks mark)))
         (hash-remove marks mark))))
            
(define empty-env (env empty (hasheq)))

(define (env-push e . vs)
  (let ([stack (env-stack e)]
        [marks (env-marks e)])
    (env (append vs stack) marks)))

(define (replicate n v)
  (if (zero? v)
      empty
      (cons v (replicate (sub1 n) v))))

(define (env-push-n e n v)
  (apply env-push e (replicate n v)))

(define (env-pop-n e n)
  (define (stack-pop-n s n)
    (cond
      [(zero? n) s]
      [(empty? s) (error 'env-pop-n "stack ran out; still need to pop ~a" n)]
      [else (stack-pop-n (rest s) (sub1 n))]))
  (let ([stack (env-stack e)]
        [marks (env-marks e)])
    (env (stack-pop-n stack) marks)))

(define (env-pop e n)
  (env-pop-n e 1))

(define (env-ref e n)
  (define (stack-ref s n)
    (cond
      [(empty? s) (error 'env-ref "stack ran out; still need slot ~a" n)]
      [(zero? n) (first s)]
      [else (stack-ref (rest s) (sub1 n))]))
  (stack-ref (env-stack e) n))
          
(define (env-set e n v)
  (define (stack-set s n v)
    (cond
      [(empty? s) (error 'env-set "stack ran out; still need slot ~a for ~a" n v)]
      [(zero? n) (cons v (rest s))]
      [else (cons (first s) (stack-set (rest s) (sub1 n) v))]))
  (let ([stack (env-stack e)]
        [marks (env-marks e)])
    (env (stack-set stack n v) marks)))

(define (env-clear e n)
  (env-set e n 'undefined))
