#lang racket

(require redex/reduction-semantics)
(require "../racket-machine/grammar.rkt"
         "../racket-machine/util.rkt")

(define-extended-language CESK bytecode
  (p (C E S K) error)
  
  (C e)
  
  (E (u ... e))
  (e ε E)
  (u v uninit (box x))
  
  (S ((x h) ...))
  (h v ((clos n (u ...) x) ...))
  
  (K ε ; push and pop frames as necessary
     (let-one e K)
     (install-value n e K)
     (install-value-box n e K)
     (application e e ... K) ; have to obey the stack discipline so stack references are correct
     (seq e e ... K)
     (branch e e K)
     (letrec (l ...) e K)))

(define stack-ref-rules
  (reduction-relation
   CESK
   (--> ((loc n) E S K)
        ((stack-ref n E) E S K)
        "loc")
   (--> ((loc-noclr n) E S K)
        ((stack-ref n S) E S K)
        "loc-noclr")
   (--> ((loc-clr n) E S K)
        ((stack-ref n E) (stack-set uninit n E) S K)
        "loc-clr")
   (--> ((loc-box n) E S K)
        ((heap-ref (stack-ref n E) S) E S K)
        "loc-box")
   (--> ((loc-box-noclr n) E S K)
        ((heap-ref (stack-ref n E) S) E S K)
        "loc-box-noclr")
   (--> ((loc-box-clr n) E S K)
        ((heap-ref (stack-ref n E) S) (stack-set uninit n E) S K)
        "loc-box-clr")))

(define stack-push-rules
  (reduction-relation
   CESK
   (--> ((let-one e_r e_b) E S K)
        (e_r (framepush (push-uninit 1 E)) S (let-one e_b K))
        "let-one")
   (--> (v E S (let-one e_b K))
        (e_b (stack-set 0 v (framepop E)) S K)
        "let-one-cont")
   (--> ((let-void n e) E S K)
        (e (push-uninit n E) S K)
        "let-void")
   (--> ((let-void-box n e) E S K)
        (e (push ((box x_n) ...) E) ((x_n undefined) ... (x_0 h_0) ...) K)
        (where (x_n ...) (n-freshes n x_0 ...))
        "let-void-box")))

(define stack-change-rules
  (reduction-relation
   CESK
   (--> ((boxenv n e) E ((x_0 h_0) ...) K)
        (e (stack-set (box x) n E) ((x v) (x_0 h_0) ...) K)
        (fresh x)
        (where v (stack-ref n E))
        "boxenv")
   (--> ((install-value n e_r e_b) E S K)
        (e_r (framepush E) S (install-value n e_b))
        "install-value")
   (--> (v E S (install-value n e_b K))
        (e_b (stack-set v n (framepop E)) S K)
        "install-value-cont")
   (--> ((install-value-box n e_r e_b) E S K)
        (e_r (framepush E) S (install-value-box n e_b K))
        "install-value-box")
   (--> (v E S (install-value-box n e_b K))
        (e_b E- (heap-set v (stack-ref n E-) S) K)
        (where E- (framepop E))
        "install-value-box-cont")))

(define-metafunction
  CESK)
  

(define ruless
  (reduction-relation
   CESK
   (--> ((application e_0 e_1 ...) E S K)
        (e_0 (framepush (push-uninit n E)) S (application e_1 ... K))
        (where n ,(length (term (e_1 ...))))
        (where (n_1 ...) (count-up n))
        "application")
   (--> ((application e_0 e_1 ...) E S K)
        (e_0 (push n (framepush E)) S (application-rator e_1 ... K))
        (where n ,(length (term e_1 ...)))
        "application-rator")
   (--> ((clos x) E ((x_0 h_0) ... (x (clos)) (x_i+1 h_i+1) ...) (application-rator K))
        ((single arg func) (framepop E) S K)
        "no-arg function")
   (--> (v E S (application-rator e_1 e_2 ... K))
        (e_1 (stack-set v n E) S (application 0 e_2 ... K))
        (where n ,(length (term e_1 e_2 ...)))
        "check")
   (--> (v E S (application i e_i+1 e_i+2 ... K))
        (e_i+1 (stack-set v n E) S (application ,(add1 (term i)) e_i+2 ... K))
        "arg eval")
   (--> (v E S (application n K))
        ((stack-ref n E) (stack-set v n E) S (call n K))
        "now apply")

   (--> ((seq e_0 e_1 e_r ...) E S K)
        (e_0 (framepush E) S (seq e_1 e_r ... K))
        "seq-many")
   (--> ((seq e_0) E S K)
        (e_0 E S K)
        "seq-one")
   (--> (v E S (seq e_1 e_r ... K))
        ((seq e_1 e_r ...) (framepop E) S K)
        "seq-cont")
   (--> ((branch e_c e_t e_f) E S K)
        (e_c (framepush E) S (branch e_t e_f K))
        "branch")
   (--> (v E S (branch e_t e_f K))
        (e_t (framepop E) S K)
        (side-condition (≠ (term v) (term #f)))
        "branch-cont-true")
   (--> (#f E S (branch e_t e_f K))
        (e_f (framepop E) S K)
        "branch-cont-false")
   (let-rec (l ...) e)
   (indirect x)
   (--> ((proc-const (τ ...) e) E ((x_i h_i) ...) K)
        ((clos x) E ((x ((clos n () e))) (x_i h_i) ...) K)
        (fresh x)
        "proc-const")
   (--> ((case-lam l ...) E S K))
   (--> ((lam (τ ...) (n ...) e) E ((x_i h_i) ...) K)
        ((clos x) E ((x ((clos n (map n stack-ref E) e))) (x_i h_i) ...) K)
        "lam"
        ((clos n (u ...) x) ...))))

(define procedure-rules
  (reduction-relation
   CESK
   (--> (V S ((x_0 h_0) ...) T ((lam n (n_0 ...) x_i) i ...))
        ((clos x) S ((x ((clos n ((stack-ref n_0 S) ...) x_i))) (x_0 h_0) ...) T (i ...))
        (fresh x)
        "lam")
   (--> (V S ((x_0 h_0) ...) T ((case-lam (lam n (n_0 ...) x_j) ...) i ...))
        ((clos x) S ((x ((clos n ((stack-ref n_0 S) ...) x_j) ...)) (x_0 h_0) ...) T (i ...))
        (fresh x)
        "case-lam")
   (--> (V S ((x_0 h_0) ...) T ((let-rec ((name l_0 (lam n_0 (n_00 ...) y_0)) ...) e) i ...))
        (V S_* ((x_0 h_0) ... (x ((clos n_0 ((stack-ref n_00 S_*) ...) y_0))) ...) T (e i ...))
        (fresh ((x ...) (l_0 ...)))
        (where (n ...) (count-up ,(length (term (l_0 ...)))))
        (where S_* (stack-set* ((clos x) n) ... S))
        "let-rec")))

;; hide the 'apply append' in a metafunction 
#;(define-metafunction runtime
  [(flatten ((any ...) ...))
   (any ... ...)])

(define application-rules
  (reduction-relation
   runtime
   (--> (V S H T ((application e_0 e_1 ...) i ...))
        (V (push-uninit n S) H T ((reorder (call n) (e_0 ?) (e_1 n_1) ...) i ...))
        (where n ,(length (term (e_1 ...))))
        (where (n_1 ...) (count-up n))
        "application")
   (--> (V S H T ((self-app x e_0 e_1 ...) i ...))
        (V S H T ((application e_0 e_1 ...) i ...))
        "self-app")
   (--> (V S H T ((self-app x e_0 e_1 ...) i ...))
        (V (push-uninit n S) H T ((reorder (self-call x) (e_1 n_1) ...) i ...))
        (where n ,(length (term (e_1 ...))))
        (where (n_1 ...) (count-up n))
        "self-app-opt")
   (--> (V S H T ((reorder i_r (e_0 m_1) ... ((loc-noclr n) m_i) (e_i+1 m_i+1) (e_i+2 m_i+2) ...) i ...))
        (V S H T ((reorder i_r (e_0 m_1) ... (e_i+1 m_i+1) (e_i+2 m_i+2) ... ((loc-noclr n) m_i)) i ...))
        "reorder")
   (--> (V S H T ((reorder (call n) (e_0 n_0) ... (e_n ?)) i ...))
        (V S H T (,@(term (flatten ((framepush e_0 framepop (set n_0)) ...)))
                  framepush e_n framepop (call n) i ...))
        "finalize-app-is-last")
   (--> (V S H T ((reorder (call n) (e_0 n_0) ... (e_i ?) (e_i+1 n_i+1) ... (e_j n_j)) i ...))
        (V S H T (,@(term (flatten ((framepush e_0 framepop (set n_0)) ...)))
                  framepush e_i framepop (set n_j)
                  ,@(term (flatten ((framepush e_i+1 framepop (set n_i+1)) ...)))
                  framepush e_j framepop
                  (swap n_j) (call n) i ...))
        "finalize-app-not-last")
   (--> (V S H T ((reorder (self-call x) (e_0 n_0) ...) i ...))
        (V S H T (,@(term (flatten ((framepush e_0 framepop (set n_0)) ...)))
                  (self-call x) i ...))
        "finalize-self-app")
   (--> ((clos x_i) (u_1 ... u_n+1 ... (u_m ... (u_k ... s))) (name H ((x_0 h_0) ...
                                                                       (x_i ((clos n_0 (u_0 ...) y_0) ...
                                                                             (clos n_i (u_i ...) y_i) 
                                                                             (clos n_i+1 (u_i+1 ...) y_i+1) ...))
                                                                       (x_i+1 h_i+1) ...)) (name T ((y_j e_j) ... (y_i e_i) (y_k e_k) ...)) ((call n_i) i ...))
        ((clos x_i) ((u_i ... (u_1 ... s))) H T (e_i i ...))
        (side-condition (not (memq (term n_i) (term (n_0 ...)))))
        (side-condition (= (term n_i) (length (term (u_1 ...)))))
        "call")
   (--> (V (u_0 ... u_i ... (u_j ... (u_k ... s))) H (name T ((x_0 e_0) ... (x_i e_i) (x_i+1 e_i+1) ...)) ((self-call x_i) i ...))
        (V ((u_j ... (u_0 ... s))) H T (e_i i ...))
        (side-condition (= (length (term (u_0 ...))) (length (term (u_k ...)))))
        "self-call")
   (--> (v S H T ((call n) i ...))
        error
        "non-closure"
        (side-condition (not (clos? (term v)))))
   (--> ((clos x_i) 
         S
         ((x_0 h_0) ... (x_i ((clos n_0 (u_0 ...) y_0) ...)) (x_i+1 h_i+1) ...)
         T
         ((call n) i ...))
        error
        (side-condition (not (memq (term n) (term (n_0 ...)))))
        "app-arity")))



(define stack-instructions
  (reduction-relation
   runtime
   (--> (v E S (set n K))
        (v (stack-set v n E) S K)
        "set")
   (--> (v E S (set-box n K))
        (v E (heap-set v (stack-ref n E) S) K)
        "set-box")
   (--> (v E S (swap n K)) ; make meta
        ((stack-ref n S) (stack-set V n S) H T (i ...))
        "swap")
   (--> (V (u_0 ... (u_1 ... (u_2 ... s))) H T (framepop i ...)) ; make meta
        (V s H T (i ...))
        "framepop")
   (--> (V S H T (framepush i ...)) ; make meta
        (V (((S))) H T (i ...))
        "framepush")))





(define-metafunction runtime
  [(n-freshes n x ... T)
   ,(variables-not-in (term (x ... T)) (build-list (term n) (λ (_) 'x)))])

(define miscellaneous-rules
  (reduction-relation
   runtime
   (--> (V S H T (v i ...))
        (v S H T (i ...))
        "value")
   (--> (V S H T ((branch e_c e_t e_f) i ...))
        (V S H T (framepush e_c framepop (branch e_t e_f) i ...))
        "branch")
   (--> (v S H T ((branch e_t e_f) i ...))
        (v S H T (e_t i ...))
        (side-condition (≠ (term v) (term #f)))
        "branch-true")
   (--> (#f S H T ((branch e_t e_f) i ...))
        (#f S H T (e_f i ...))
        "branch-false")
   (--> (V S H T ((seq e_1 e_2 e_3 e_4 ...) i ...))
        (V S H T (framepush e_1 framepop (seq e_2 e_3 e_4 ...) i ...))
        "seq-many")
   (--> (V S H T ((seq e_1 e_2) i ...))
        (V S H T (framepush e_1 framepop e_2 i ...))
        "seq-two")
   (--> (V S H (name T ((x_0 e_0) ... (x_i e_i) (x_i+1 e_i+1) ...)) ((indirect x_i) i ...))
        (V S H T (e_i i ...))
        "indirect")))

(define ->
  (union-reduction-relations
   stack-ref-rules
   stack-instructions
   stack-push-rules
   stack-change-rules
   procedure-rules
   application-rules
   miscellaneous-rules))

(define (≠ a b) (not (equal? a b)))

(define clos? 
  (redex-match runtime (clos x)))

(define-metafunction
  CESK
  heap-ref : (box x) H -> h
  [(heap-ref (box x_i) ((x_0 h_0) ... (x_i h_i) (x_i+1 h_i+1) ...)) h_i])

(define-metafunction
  CESK
  heap-set : h (box x) H -> H
  [(heap-set h (box x_i) ((x_0 h_0) ... (x_i h_i) (x_i+1 h_i+1) ...))
   ((x_0 h_0) ... (x_i h) (x_i+1 h_i+1) ...)])

(define-metafunction
  CESK
  push : (u ...) (u ... s) -> (u ... s)
  [(push (u_0 ...) (u_i ... s))
   (u_0 ... u_i ... s)])

(define-metafunction
  CESK
  push-uninit : n (u ... s) -> (uninit ... u ... s)
  [(push-uninit 0 S) S]
  [(push-uninit n (u ... s))
   (push-uninit ,(- (term n) (term 1)) (uninit u ... s))])

(define-metafunction
  CESK
  stack-ref : n S -> u
  [(stack-ref 0 (v u ... s)) v]
  [(stack-ref 0 ((box x) u ... s)) (box x)]
  [(stack-ref n (u_0 u_1 ... s)) 
   (stack-ref ,(- (term n) (term 1)) (u_1 ... s))
   (side-condition (> (term n) (term 0)))]
  [(stack-ref n ((u ... s)))
   (stack-ref n (u ... s))])

(define-metafunction
  CESK
  framepush : S -> S
  [(framepush s) (((s)))])

(define-metafunction
  CESK
  framepop : S -> S
  [(framepop (u_0 ... (u_1 ... (u_2 ... s)))) s])

(define-metafunction
  CESK
  stack-set : u n S -> S
  [(stack-set u n (u_0 ... u_n u_n+1 ... s))
   (u_0 ... u u_n+1 ... s)
   (side-condition 
    (= (term n) (length (term (u_0 ...)))))]
  [(stack-set u n (u_0 ... s))
   (u_0 ... (stack-set u ,(- (term n) (length (term (u_0 ...)))) s))])

(define-metafunction
  CESK
  stack-set* : (u n) ... S -> S
  [(stack-set* S) S]
  [(stack-set* (u_0 n_0) (u_1 n_1) ... S)
   (stack-set* (u_1 n_1) ... (stack-set u_0 n_0 S))])

(provide (all-defined-out))
