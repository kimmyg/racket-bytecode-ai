#lang racket/base
(require "C.rkt"
         "E.rkt"
         "S.rkt"
         "K.rkt")

(struct cesk (cont env store kont) #:transparent)

(define step
  (match-lambda
    [(or (cesk (loc n) env store kont)
         (cesk (loc-noclr n) env store kont))
     (cesk (env-lookup env n) env store kont)]
    [(cesk (loc-clr n) env store kont)
     (cesk (env-lookup env n) (env-clear env n) store kont)]
    [(or (cesk (loc-box n) env store kont)
         (cesk (loc-box-noclr n) env store kont))
     (cesk (store-ref store (env-ref env n)) env store kont)]
    [(cesk (loc-box-clr n) env store kont)
     (cesk (store-ref store (env-ref env n)) (env-clear env n) store kont)]
    [(cesk (let-one e0 e1) env store kont)
     (let ([env (env-push env 1)])
       (cesk e0 env store (let-one-k env kont e1)))]
    [(cesk (let-void n e) env store kont)
     (cesk e (env-push env n) store kont)]
    [(cesk (let-void-box n e) env store kont)
     (let-values ([(env store) (for/fold ([env env]
                                          [store store])
                                 ([i n])
                                 (let-values ([(store address) (store-alloc store)])
                                   (values (env-push env 1 address) store)))])
       (cesk e env store kont))]
    [(cesk (boxenv n e) env store kont)
     (let-values ([(store address) (store-set store (env-ref env n))])
       (cesk e (env-set env n address) store kont))]
    [(cesk (install-value n e0 e1) env store kont)
     (cesk e0 env store `(install-value env kont n e1))]
    [(cesk (install-value-box n e0 e1) env store kont)
     (cesk e0 env store (install-value-box-k env kont n e1))]
    [(cesk (application e0 es ...) env store kont)
     (let* ([n (length es)]
            [env (env-push env n)])
       (cesk e0 env store (application-k env kont es n)))]
    [(cesk (? value? v) _ store (let-one-k env kont e1))
     (cesk e1 (env-set env 0 v) store kont)]
    [(cesk (? value? v) _ store (install-value-k env kont n e1))
     (cesk e1 (env-set env n v) store kont)]
    [(cesk (? value? v) _ store (install-value-box-k env kont n e1))
     (cesk e1 env (store-set store (env-get env n) v) kont)]
    [(cesk (? value? v) _ store (application-k env kont es n))
     (if (empty? es)
         (perform application)
         (env-set env (sub1 (length es)) v))]
    [(cesk (seq e0 e1 es ...) env store kont)
     #f]
    [(cesk (branch e0 e1 e2) env store kont)
     (cesk e0 env store (branch-k env kont e1 e2))]
    [(cesk (let-rec ()))
     #f]
    [(cesk (indirect x) ())
     #f]
    [(cesk (proc-const (list (? tau? ts) ...) e) ())
     #f]
    [(cesk (case-lam (list (? lam? ls) ...)) ())
     #f]
    [(cesk (? lam? l) env store kont)
     #f]