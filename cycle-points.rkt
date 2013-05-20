#lang racket/base
(require racket/match
         racket/set
         compiler/zo-structs)

(provide determine-cycle-points)

(define (determine-cycle-points e)
  (define (inner e [seen-count (hasheq)])
    (if (zo? e)
        (let ([seen-count (hash-update seen-count e add1 0)])
          (if (> (hash-ref seen-count e 0) 1)
              (begin
                (printf "seen twice ~a\n" e)
              (if (closure? e)
                  seen-count
                  (error 'determine-cycle-points "illegal cycle")))
              (match e
                [(compilation-top _ _ e)
                 (inner e seen-count)]
                [(def-values es e)
                 (inner e (foldl inner seen-count es))]
                [(inline-variant e0 e1)
                 (inner e1 (inner e0 seen-count))]
                [(seq es)
                 (foldl inner seen-count es)]
                [(mod _ _ _ _ _ _ es _ _ _ _ _ _ _ _ _)
                 (foldl inner seen-count es)]
                [(lam name _ _ _ _ _ _ _ _ e)
                 (inner e seen-count)]
                [(closure e _)
                 (inner e seen-count)]
                [(case-lam _ es)
                 (foldl inner seen-count es)]
                [(let-one e0 e1 _ _)
                 (inner e1 (inner e0 seen-count))]
                [(let-void _ _ e)
                 (inner e seen-count)]
                [(install-value _ _ _ e0 e1)
                 (inner e1 (inner e0 seen-count))]
                [(let-rec es e)
                 (inner e (foldl inner seen-count es))]
                [(boxenv _ e)
                 (inner e seen-count)]
                [(localref _ _ _ _ _)
                 seen-count]
                [(toplevel _ _ _ _)
                 seen-count]
                [(topsyntax _ _ _)
                 seen-count]
                [(application e es)
                 (foldl inner (inner e seen-count) es)]
                [(branch e0 e1 e2)
                 (inner e2 (inner e1 (inner e0 seen-count)))]
                [(with-cont-mark e0 e1 e2)
                 (inner e2 (inner e1 (inner e0 seen-count)))]
                [(beg0 es)
                 (foldl inner seen-count es)]
                [(varref _ _)
                 seen-count]
                [(assign _ e _)
                 (inner e seen-count)]
                [(apply-values e0 e1)
                 (inner e1 (inner e0 seen-count))]
                [(primval _)
                 seen-count])))
        seen-count))
  (for/fold ([cps (seteq)])
    ([(e n) (inner e)])
    (if (> n 1)
        (set-add cps e)
        cps)))
