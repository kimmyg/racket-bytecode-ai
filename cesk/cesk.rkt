#lang racket

(require redex/reduction-semantics)
(require "grammar.rkt" "util.rkt")

(define-language bytecode
  (e (loc n)
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
     (proc-const (τ ...) e)
     (case-lam l ...)
     l
     v)
  
  (l (lam (τ ...) (n ...) e))
  
  (v number
     void
     'variable
     b)
  
  (τ val ref)
  (n natural)
  (b #t #f)
  ((x y) variable))

(define-extended-language CESK bytecode
  (p (C E S K) error)
  
  (C e)
  
  (E (u ... e))
  (e ε E)
  (u v uninit (box x))
  
  (S ((x h) ...))
  (h e ((clos n (u ...) x) ...))
  
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

(define ruless
  (reduction-relation
   CESK
   (--> ((application e_0 e_1 ...) E S K)
        (e_0 (framepush (push-uninit n E)) S (application e_1 ... K))
        (where n ,(length (term (e_1 ...))))
        (where (n_1 ...) (count-up n))
        "application"
    (application e e ...)
     (seq e e e ...)
     (branch e e e)
     (let-rec (l ...) e)
     (indirect x)
     (proc-const (τ ...) e)
     (case-lam l ...)
     l

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
(define-metafunction runtime
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

(define-extended-language loader bytecode
  (φ - (n n x))
  (e any)
  (H any)
  (h any)
  (T any)
  (l any))

(define-metafunction loader
  [(load e ((x_0 (proc-const (τ ...) e_b)) ...))
   (uninit (((ε))) H (concat ((x_0 e_0*) ...) T) (e_*))
   (where ((e_* e_0* ...) H T (y ...))
          (load’* ((e -) ((proc-const (τ ...) e_b) -) ...)
                  (x_0 ...)))])

(define-metafunction loader
  [(incφ - n) -]
  [(incφ (n_p n_a x) n) (,(+ (term n) (term n_p)) n_a x)])

(define-metafunction loader
  [(load-lam-rec (lam (τ_0 ...) (n_0 ... n_i n_i+1 ...) e) n_i (y ...))
   ; When a closure captures itself multiple times, only the last 
   ; occurrence is considered a self-reference.
   ((lam n (n_0 ... n_i n_i+1 ...) x) H (concat ((x e_*)) T) (y_* ...))
   (where n ,(length (term (τ_0 ...))))
   (where x (fresh-in (y ...)))
   (where (e_* H T (y_* ...))
          (load’ e (,(length (term (n_0 ...))) n x) (x y ...)))
   (side-condition (not (memq (term n_i) (term (n_i+1 ...)))))]
  [(load-lam-rec l n_j (y ...)) (load’ l - (y ...))])

(define-metafunction loader
  [(load-lam-rec* () (y ...)) (() () () (y ...))]
  [(load-lam-rec* ((l_0 n_0) (l_1 n_1) ...) (y ...))
   ((l_0* l_1* ...) (concat H_0 H_1) (concat T_0 T_1) (y_** ...))
   (where (l_0* H_0 T_0 (y_* ...))
          (load-lam-rec l_0 n_0 (y ...)))
   (where ((l_1* ...) H_1 T_1 (y_** ...))
          (load-lam-rec* ((l_1 n_1) ...) (y_* ...)))])

(define-metafunction loader
  [(load’ (application (loc-noclr n) e_1 ...) (n_p n_a x) (y ...))
   ((self-app x (loc-noclr n) e_1* ...) H T (y_* ...))
   (side-condition (= (term n) (+ (term n_p) (length (term (e_1 ...))))))
   (side-condition (= (term n_a) (length (term (e_1 ...)))))
   (where ((e_1* ...) H T (y_* ...)) (load’* ((e_1 -) ...) (y ...)))]
  
  [(load’ (let-rec (l_0 ...) e) φ (y ...))
   ((let-rec (l_0* ...) e_*)
    (concat H_0 H_1) (concat T_0 T_1)
    (y_** ...))
   (where (e_* H_0 T_0 (y_* ...)) (load’ e φ (y ...)))
   (where (n_0 ...) (count-up ,(length (term (l_0 ...)))))
   (where ((l_0* ...) H_1 T_1 (y_** ...)) 
          (load-lam-rec* ((l_0 n_0) ...) (y_* ...)))]
  
  [(load’ (application e_0 e_1 ...) φ (y ...))
   ((application e_0* e_1* ...) H T (y_* ...))
   (where ((e_0* e_1* ...) H T (y_* ...))
          (load’* ((e_0 -) (e_1 -) ...) (y ...)))]
  
  [(load’ (let-one e_r e_b) φ (y ...))
   ((let-one e_r* e_b*) 
    (concat H_r H_b) (concat T_r T_b)
    (y_** ...))
   (where (e_r* H_r T_r (y_* ...)) (load’ e_r - (y ...)))
   (where (e_b* H_b T_b (y_** ...)) (load’ e_b (incφ φ 1) (y_* ...)))]
  
  [(load’ (let-void n e) φ (y ...))
   ((let-void n e_*) H T (y_* ...))
   (where (e_* H T (y_* ...)) (load’ e (incφ φ n) (y ...)))]
  [(load’ (let-void-box n e) φ (y ...))
   ((let-void-box n e_*) H T (y_* ...))
   (where (e_* H T (y_* ...)) (load’ e (incφ φ n) (y ...)))]
  
  [(load’ (boxenv n e) φ (y ...))
   ((boxenv n e_*) H T (y_* ...))
   (where (e_* H T (y_* ...)) (load’ e φ (y ...)))]
  
  [(load’ (install-value n e_r e_b) φ (y ...))
   ((install-value n e_r* e_b*)
    (concat H_r H_b) (concat T_r T_b)
    (y_** ...))
   (where (e_r* H_r T_r (y_* ...)) (load’ e_r - (y ...)))
   (where (e_b* H_b T_b (y_** ...)) (load’ e_b φ (y_* ...)))]
  [(load’ (install-value-box n e_r e_b) φ (y ...))
   ((install-value-box n e_r* e_b*)
    (concat H_r H_b) (concat T_r T_b)
    (y_** ...))
   (where (e_r* H_r T_r (y_* ...)) (load’ e_r - (y ...)))
   (where (e_b* H_b T_b (y_** ...)) (load’ e_b φ (y_* ...)))]
  
  [(load’ (seq e_0 ... e_n) φ (y ...))
   ((seq e_0* ... e_n*) 
    (concat H_0 H_1) (concat T_0 T_1)
    (y_** ...))
   (where ((e_0* ...) H_0 T_0 (y_* ...)) (load’* ((e_0 -) ...) (y ...)))
   (where (e_n* H_1 T_1 (y_** ...)) (load’ e_n φ (y_* ...)))]
  
  [(load’ (branch e_c e_t e_f) φ (y ...))
   ((branch e_c* e_t* e_f*)
    (concat H_c H_t H_f) (concat T_c T_t T_f )
    (y_*** ...))
   (where (e_c* H_c T_c (y_* ...)) (load’ e_c - (y ...)))
   (where (e_t* H_t T_t (y_** ...)) (load’ e_t φ (y_* ...)))
   (where (e_f* H_f T_f (y_*** ...)) (load’ e_f φ (y_** ...)))]
  
  [(load’ (lam (τ_0 ...) (n_0 ...) e) φ (y ...))
   ((lam n (n_0 ...) x) 
    H 
    (concat ((x e_*)) T)
    (y_* ...))
   (where x (fresh-in (y ...)))
   (where n ,(length (term (τ_0 ...))))
   (where (e_* H T (y_* ...)) (load’ e - (x y ...)))]
  
  [(load’ (proc-const (τ_0 ...) e) φ (y ...))
   ((clos x) 
    (concat ((x ((clos n () x_*)))) H) 
    (concat ((x_* e_*)) T)
    (y_* ...))
   (where x (fresh-in (y ...)))
   (where x_* (fresh-in (x y ...)))
   (where n ,(length (term (τ_0 ...))))
   (where (e_* H T (y_* ...)) (load’ e - (x x_* y ...)))]
  
  [(load’ (case-lam l_0 ...) φ (y ...))
   ((case-lam l_0* ...) H T (y_* ...))
   (where ((l_0* ...) H T (y_* ...))
          (load’* ((l_0 φ) ...) (y ...)))]
  
  [(load’ e φ (y ...)) (e () () (y ...))])

(define-metafunction loader
  [(load’* () (y ...)) (() () () (y ...))]
  [(load’* ((e_0 φ_0) (e_1 φ_1) ...) (y ...))
   ((e_0* e_1* ...) 
    (concat H_0 H_1) (concat T_0 T_1)
    (y_** ...))
   (where (e_0* H_0 T_0 (y_* ...)) 
          (load’ e_0 φ_0 (y ...)))
   (where ((e_1* ...) H_1 T_1 (y_** ...))
          (load’* ((e_1 φ_1) ...) (y_* ...)))])

(define-metafunction loader
  [(fresh-in (x ...))
   ,(variable-not-in (term (x ...)) 'x)])

(define-metafunction
  runtime
  heap-ref : (box x) H -> h
  [(heap-ref (box x_i) ((x_0 h_0) ... (x_i h_i) (x_i+1 h_i+1) ...)) h_i])

(define-metafunction
  runtime
  heap-set : h (box x) H -> H
  [(heap-set h (box x_i) ((x_0 h_0) ... (x_i h_i) (x_i+1 h_i+1) ...))
   ((x_0 h_0) ... (x_i h) (x_i+1 h_i+1) ...)])

(define-metafunction
  runtime
  push : (u ...) (u ... s) -> (u ... s)
  [(push (u_0 ...) (u_i ... s))
   (u_0 ... u_i ... s)])

(define-metafunction
  runtime
  push-uninit : n (u ... s) -> (uninit ... u ... s)
  [(push-uninit 0 S) S]
  [(push-uninit n (u ... s))
   (push-uninit ,(- (term n) (term 1)) (uninit u ... s))])

(define-metafunction
  runtime
  stack-ref : n S -> u
  [(stack-ref 0 (v u ... s)) v]
  [(stack-ref 0 ((box x) u ... s)) (box x)]
  [(stack-ref n (u_0 u_1 ... s)) 
   (stack-ref ,(- (term n) (term 1)) (u_1 ... s))
   (side-condition (> (term n) (term 0)))]
  [(stack-ref n ((u ... s)))
   (stack-ref n (u ... s))])

(define-metafunction
  runtime
  stack-set : u n S -> S
  [(stack-set u n (u_0 ... u_n u_n+1 ... s))
   (u_0 ... u u_n+1 ... s)
   (side-condition 
    (= (term n) (length (term (u_0 ...)))))]
  [(stack-set u n (u_0 ... s))
   (u_0 ... (stack-set u ,(- (term n) (length (term (u_0 ...)))) s))])

(define-metafunction
  runtime
  stack-set* : (u n) ... S -> S
  [(stack-set* S) S]
  [(stack-set* (u_0 n_0) (u_1 n_1) ... S)
   (stack-set* (u_1 n_1) ... (stack-set u_0 n_0 S))])

(provide (all-defined-out))