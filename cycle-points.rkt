#lang racket/base
(require racket/match
         racket/set
         compiler/zo-structs)

(provide determine-cycle-points)

(define (determine-cycle-points e [cps (seteq)])
  (if (zo? e)
      (if (set-member? cps e)
          (if (closure? e)
              cps
              (error 'determine-cycle-points "illegal cycle"))
          (let ([cps (set-add cps e)])
            (match e
              [(compilation-top _ _ e)
               (determine-cycle-points e cps)]
              [(def-values es e)
               (determine-cycle-points e (foldl determine-cycle-points cps es))]
              [(inline-variant e0 e1)
               (determine-cycle-points e1 (determine-cycle-points e0 cps))]
              [(seq es)
               (foldl determine-cycle-points cps es)]
              [(mod _ _ _ _ _ _ es _ _ _ _ _ _ _ _)
               (foldl determine-cycle-points cps es)]
              [(lam _ _ _ _ _ _ _ _ _ e)
               (determine-cycle-points e cps)]
              [(closure e _)
               (determine-cycle-points e cps)]
              [(case-lam _ es)
               (foldl determine-cycle-points cps es)]
              [(let-one e0 e1 _ _)
               (determine-cycle-points e1 (determine-cycle-points e0 cps))]
              [(let-void _ _ e)
               (determine-cycle-points e cps)]
              [(install-value _ _ _ e0 e1)
               (determine-cycle-points e1 (determine-cycle-points e0 cps))]
              [(let-rec es e)
               (determine-cycle-points e (foldl determine-cycle-points cps es))]
              [(boxenv _ e)
               (determine-cycle-points e cps)]
              [(localref _ _ _ _ _)
               cps]
              [(toplevel _ _ _ _)
               cps]
              [(topsyntax _ _ _)
               cps]
              [(application e es)
               (foldl determine-cycle-points (determine-cycle-points e cps) es)]
              [(branch e0 e1 e2)
               (determine-cycle-points e2 (determine-cycle-points e1 (determine-cycle-points e0 cps)))]
              [(with-cont-mark e0 e1 e2)
               (determine-cycle-points e2 (determine-cycle-points e1 (determine-cycle-points e0 cps)))]
              [(beg0 es)
               (foldl determine-cycle-points cps es)]
              [(varref _ _)
               cps]
              [(assign _ e _)
               (determine-cycle-points e cps)]
              [(apply-values e0 e1)
               (determine-cycle-points e1 (determine-cycle-points e0 cps))]
              [(primval _)
               cps])))  
      cps))
