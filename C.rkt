#lang racket/base

(struct)

(struct e () #:transparent)

(struct loc e (n) #:transparent)
(struct loc-noclr e (n) #:transparent)
(struct loc-clr e (n) #:transparent)
(struct loc-box e (n) #:transparent)
(struct loc-box-noclr e (n) #:transparent)
(struct loc-box-clr e (n) #:transparent)

(struct let-one e (e0 e1) #:transparent)
(struct let-void e (n e) #:transparent)
(struct let-void-box e (n e) #:transparent)

(struct boxenv e (n e) #:transparent)
(struct install-value e (n e0 e1) #:transparent)
(struct install-value-box e (n e0 e1) #:transparent)

(struct application e ())
(struct seq e ())
(struct branch e (e0 e1 e2) #:transparent)
(struct let-rec e (lams e))
(struct indirect e (x))
(struct proc-const e (taus e))
(struct case-lam e (lams) #:transparent)
(struct lam e (pars clos))
  