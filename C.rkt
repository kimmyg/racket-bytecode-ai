#lang racket/base

(provide e?
         l?
         v?
         t?
         n?
         b?
         x?)

(define e-keyword?
  (match-lambda
    [(or 'loc 'loc-noclr 'loc-clr 'loc-box 'loc-box-noclr 'loc-box-clr
         'let-one 'let-void 'let-void-box
         'boxenv 'install-value 'install-value-box
         'application 'seq 'branch 'let-rec 'indirect 'proc-const 'case-lam) #t]
    [_ #f]))

(define e?
  (match-lambda
    [(list (? e-keyword? _) _ ...) #t]
    [_ #f]))

(define l?
  (match-lambda
    [(list 'lam
           (list (? t? ts) ...)
           (list (? n? ns) ...)
           (? e? e)) #t]
    [_ #f]))



(provide e?
         l?
         v?
         t?
         n?
         b?
         x?)



(loc n)
(loc-noclr n)
(loc-clr n)
(loc-box n)
(loc-box-noclr n)
(loc-box-clr n)

(let-one e e)
(let-void n e)
(let-void-box n e)

(boxenv n e)
(install-value n e e)
(install-value-box n e e)

(application e e ...)
(seq e e e ...)
(branch e e e)
(let-rec (l ...) e)
(indirect x)
(proc-const (t ...) e)
(case-lam l ...)

(lam (t ...) (n ...) e)

(match e
  [(branch e0 e1 e2)]
  [(list 'branch e0 e1 e2)])

(match e
  [(application e0 es ...)]
  [(list 'application e0 es ...)

(define e-keyword?
  (match-lambda
    [(or 'loc 'loc-noclr 'loc-clr 'loc-box 'loc-box-noclr 'loc-box-clr
         'let-one 'let-void 'let-void-box
         'boxenv 'install-value 'install-value-box
         'application 'seq 'branch 'let-rec 'indirect 'proc-const 'case-lam) #t]
    [_ #f]))

(define e?
  (match-lambda
    [(list (? e-keyword? _) _ ...) #t]
    [_ #f]))

(define l?
  (match-lambda
    [(list 'lam
           (list (? t? ts) ...)
           (list (? n? ns) ...)
           (? e? e)) #t]
    [_ #f]))

(define v? (or/c number? void? x? b?))

(define t? (or/c 'val 'ref))

(define n? exact-nonnegative-number?)

(define b? boolean?)

(define x? symbol?)
  