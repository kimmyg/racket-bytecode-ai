#lang racket/base
(require compiler/zo-structs)

(provide indirect
         proc-const
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

(struct indirect expr (address))
(struct proc-const expr (name flags num-params param-types rest? toplevel-map max-let-depth body))

(struct instr expr ())
(struct swap instr (n))
(struct reorder instr (rator rands))
(struct set instr (n))
(struct set-box instr (n))
(struct branch2 instr (con alt))
(struct framepush instr ())
(struct framepop instr ())
(struct call instr (arity))
(struct self-call instr (address))

(struct clos (arity rest? captured address))

(struct clos-v (address))