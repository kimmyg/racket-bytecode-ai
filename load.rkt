#lang racket/base
(require racket/list
         racket/match
         racket/vector
         compiler/zo-structs
         "extensions.rkt"
         (only-in "stack.rkt" empty-stack)
         (prefix-in heap- (except-in "map.rkt" map))
         (prefix-in text- (except-in "map.rkt" map))
         "zip.rkt")

(provide load)

(define (φ+ φ n)
  (and φ (cons (+ (car φ) n) (cdr φ))))

(define (load e T)
  (let*-values ([(ys es) (text-extract T)]
                [(e*-es* H T) (load′* (zip (cons e es) #f))])
    (values 'uninit
            empty-stack
            H
            (text-concat (text-init ys (rest e*-es*)) T)
            (list (first e*-es*)))))

(require (prefix-in r: racket/set))
(define seen (r:seteq))

(define (load′ e φ)
  ;(printf "load ~a\n" e)
  (if (r:set-member? seen e)
      (error 'load′ "seen that e twice")
      (set! seen (r:set-add seen e)))
  (match e
    [(and v
          (or (? void?)
              (? number?)
              (? string?)
              (? symbol?)))
     (values v heap-empty text-empty)]
    [(application (localref #f n #f #f #f) rands)
     (=> fall-through)
     (let ([m (length rands)])
       (if (and φ
                (= n (+ (car φ) m))
                (= (cadr φ) m))
           (let-values ([(rands* H T) (load′* (zip rands #f))])
             (values (self-app (caddr φ) (localref n #f #f #f #f) rands*) H T))
           (fall-through)))]
    [(application rator rands)
     (let-values ([(rator* H-rator T-rator) (load′ rator #f)]
                  [(rands* H-rands T-rands) (load′* (zip rands #f))])
       (values (application rator* rands*)
               (heap-concat H-rator H-rands)
               (text-concat T-rator T-rands)))]
    [(apply-values proc args-expr)
     (let-values ([(proc* H-proc T-proc) (load′ proc φ)]
                  [(args-expr* H-args-expr T-args-expr) (load′ args-expr φ)])
       (values (apply-values proc* args-expr*)
               (heap-concat H-proc H-args-expr)
               (text-concat T-proc T-args-expr)))]
    [(branch test then else)
     (let-values ([(test* H-test T-test) (load′ test #f)]
                  [(then* H-then T-then) (load′ then φ)]
                  [(else* H-else T-else) (load′ else φ)])
       (values (branch test* then* else*)
               (heap-concat H-test H-then H-else)
               (heap-concat T-test T-then T-else)))]
    [(case-lam name clauses)
     (let-values ([(clauses* H T) (load′* (zip clauses #f))])
       (values (case-lam name clauses*) H T))]
    #;[(and e (closure code gen-id))
     (let ([proc (cons 'proc-const (rest (inner code)))])
       (if (set-member? cycle-points e)
           (indirect (or (hash-ref clos-address e #f)
                           (let ([addr (genaddr)])
                             (hash-set! clos-address e addr)
                             (hash-set! text-segment addr proc)
                             addr)))
           proc))]
    [(compilation-top max-let-depth prefix code)
     (let-values ([(code* H T) (load′ code #f)])
       (values (compilation-top max-let-depth prefix code*) H T))]
    [(def-values ids e)
     (let-values ([(e* H T) (load′ e #f)])
       (values (def-values ids e*) H T))]
    [(and e (indirect x))
     (values e heap-empty text-empty)]
    [(lam name flags num-params param-types rest? closure-map
           closure-types toplevel-map max-let-depth body)
     (let ([x (gensym 'addr)])
       (let-values ([(body* H T) (load′ e #f)])
         (values (lam name flags num-params param-types rest? closure-map
                      closure-types toplevel-map max-let-depth x)
                 H
                 (text-set T x body*))))]
    [(let-one rhs body type unused?)
     (let-values ([(rhs* H-rhs T-rhs) (load′ rhs #f)]
                  [(body* H-body T-body) (load′ body φ)])
       (values (let-one rhs* body* type unused?)
               (heap-concat H-rhs H-body)
               (text-concat T-rhs T-body)))]
    [(and e (localref unbox? pos clear? other-clears? type))
     (values e heap-empty text-empty)]
    [(mod name srcname self-modidx prefix provides requires body 
          syntax-bodies unexported max-let-depth dummy lang-info 
          internal-context flags pre-submodules post-submodules)
     (let-values ([(body* H T) (load′* (zip body #f))])
       (values (mod name srcname self-modidx prefix provides requires body*
                    syntax-bodies unexported max-let-depth dummy lang-info 
                    internal-context flags pre-submodules post-submodules)
               H
               T))]
    [(and e (primval id))
     (values e heap-empty text-empty)]
    [(proc-const name flags num-params param-types rest? toplevel-map max-let-depth body)
     (let ([x (gensym 'addr)]
           [x* (gensym 'addr)])
       (let-values ([(body* H T) (load′ body φ)])
         (values (clos-v x)
                 (heap-set H x (list (clos num-params rest? empty x*)))
                 (text-set T x* body*))))]
    [(seq (list es ... e))
     (let-values ([(es* H T) (load′* (append (zip es #f) (list (cons e φ))))])
       (values (seq es*) H T))]
    [(and e (toplevel depth pos const? ready?))
     (values e heap-empty text-empty)]))

(define (load′* e×φ-s)
  (if (empty? e×φ-s)
      (values empty heap-empty text-empty)
      (match-let ([(cons e φ) (first e×φ-s)])
        (let-values ([(e* H0 T0) (load′ e φ)]
                     [(es* H1 T1) (load′* (rest e×φ-s))])
          (values (cons e* es*) (heap-concat H0 H1) (text-concat T0 T1))))))


(define (load-lam-rec l n)
  (match l
    [(lam name flags num-params param-types rest? closure-map
          closure-types toplevel-map max-let-depth body)
     (=> fall-through)
     (let ([depth (vector-member n closure-map)])
       (if depth
           (let ([x (gensym 'addr)]) ; XXX is num-params necessary? it's not in the paper.
             (let-values ([(body* H T) (load body (cons (+ num-params depth) (cons num-params x)))])
               (values (lam name flags num-params param-types rest? closure-map
                            closure-types toplevel-map max-let-depth x)
                       H
                       (text-set T x body*))))
         (fall-through)))]
    [l
     (load′ l #f)]))

(define (load-lam-rec* ls)
  (let-values
      ([(ls* H T)
        (for/fold ([ls* empty]
                   [H heap-empty]
                   [T text-empty])
          ([l ls]
           [n (in-naturals)])
          (let-values ([(l* H* T*) (load-lam-rec l n)])
            (values (cons l* ls*)
                    (heap-concat H H*)
                    (text-concat T T*))))])
    (values (reverse ls*) H T)))
