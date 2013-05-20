#lang racket/base
(require compiler/zo-structs)

(provide indirect
         proc-const)

(struct indirect expr (address) #:transparent)
(struct proc-const expr (name flags num-params param-types rest? toplevel-map max-let-depth body) #:transparent)
