#lang racket/base
(require compiler/zo-structs
         "init-extensions.rkt")

(provide indirect
         proc-const
         self-app
         
         swap
         reorder
         set
         set-box
         branch2
         framepush
         framepop
         call
         self-call
         clos
         clos-v)


(struct self-app expr (address rator rands))

(struct instr expr () #:transparent)
(struct swap instr (slot) #:transparent)
(struct reorder instr (call rands) #:transparent)
(struct set instr (n) #:transparent)
(struct set-box instr (n))
(struct branch2 instr (con alt))
(struct framepush instr () #:transparent)
(struct framepop instr () #:transparent)
(struct call instr (arity) #:transparent)
(struct self-call instr (address))

(struct clos (arity rest? captured address) #:transparent)

(struct clos-v (address) #:transparent)