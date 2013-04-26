#lang racket/base
(require racket/list
         racket/match
         racket/set
         compiler/zo-structs
         "extensions.rkt"
         "cycle-points.rkt"
         (prefix-in text- "map.rkt"))

(provide init)

(define (init e)
  (define cycle-points (determine-cycle-points e))
  (define clos-address (make-hasheq))
  (define text-segment (make-hasheq))
  (define inner
    (match-lambda
      [(and v
            (or (? void?)
                (? number?)
                (? string?)
                (? symbol?)))
       v]
      [(application rator rands)
       (application (inner rator) (map inner rands))]
      [(apply-values proc args-expr)
       (apply-values (inner proc) (inner args-expr))]
      [(branch test then else)
       (branch (inner test) (inner then) (inner else))]
      [(case-lam name clauses)
       (case-lam name (map inner clauses))]
      [(and e (closure code gen-id))
       (let ([proc (match-let ([(lam name flags num-params param-types rest? closure-map
                                     closure-types toplevel-map max-let-depth body)
                                (inner code)])
                     (proc-const name flags num-params param-types rest?
                                 toplevel-map max-let-depth body))])
         (if (set-member? cycle-points e)
             (indirect (or (hash-ref clos-address e #f)
                             (let ([addr (gensym 'addr)])
                               (hash-set! clos-address e addr)
                               (hash-set! text-segment addr proc)
                               addr)))
             proc))]
      [(compilation-top max-let-depth prefix code)
       (compilation-top max-let-depth prefix (inner code))]
      [(def-values ids rhs)
       (def-values (map inner ids) (inner rhs))]
      [(inline-variant direct inline)
       (inner direct)]
      [(lam name flags num-params param-types rest? closure-map
            closure-types toplevel-map max-let-depth body)
       (lam name flags num-params param-types rest? closure-map
            closure-types toplevel-map max-let-depth (inner body))]
      [(let-one rhs body type unused?)
       (let-one (inner rhs) (inner body) type unused?)]
      [(and e (localref unbox? pos clear? other-clears? type))
       e]
      [(mod name srcname self-modidx prefix provides requires body 
            syntax-bodies unexported max-let-depth dummy lang-info 
            internal-context pre-submodules post-submodules)
       (mod name srcname self-modidx prefix provides requires (map inner body)
            syntax-bodies unexported max-let-depth dummy lang-info 
            internal-context pre-submodules post-submodules)]
      [(and e (primval id))
       e]
      [(seq forms)
       (seq (map inner forms))]
      [(and e (toplevel depth pos const? ready?))
       e]
      [f (error 'init "unknown form ~a" f)]))
  (values (inner e) (text-map (make-immutable-hash (hash->list text-segment)))))
