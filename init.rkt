#lang racket/base
(require racket/list
         racket/match
         racket/set
         compiler/zo-structs
         "cycle-points.rkt")

(provide init)

(define (init e)
  (define cycle-points (determine-cycle-points e))
  (define clos-address (make-hasheq))
  (define text-segment (make-hasheq))
  (define genaddr
    (let ([loc 0])
      (λ ()
        (let ([addr (string->symbol (format "addr~a" loc))])
          (set! loc (add1 loc))
          addr))))
  (define inner
    (match-lambda
      [(? void?)
       'void]
      [(and v
            (or (? number?)
                (? string?)
                (? symbol?)))
       v]
      [(application rator rands)
       `(application ,(inner rator) ,@(map inner rands))]
      [(apply-values proc args-expr)
       `(apply-values ,(inner proc) ,(inner args-expr))]
      [(branch test then else)
       `(branch ,(inner test) ,(inner then) ,(inner else))]
      [(case-lam name clauses)
       `(case-lam name ,(map inner clauses))]
      [(and e (closure code gen-id))
       (let ([proc (cons 'proc-const (rest (inner code)))])
         (if (set-member? cycle-points e)
             `(indirect ,(or (hash-ref clos-address e #f)
                             (let ([addr (genaddr)])
                               (hash-set! clos-address e addr)
                               (hash-set! text-segment addr proc)
                               addr)))
             proc))]
    [(compilation-top max-let-depth prefix code)
     `(compilation-top ,max-let-depth ,prefix ,(inner code))]
    [(def-values ids rhs)
     `(def-values ,(map inner ids) ,(inner rhs))]
    [(inline-variant direct inline)
     (inner direct)]
    [(lam name flags num-params param-types rest? closure-map
          closure-types toplevel-map max-let-depth body)
     `(lam ,name ,flags ,param-types ,rest? ,closure-map ,closure-types
           ,toplevel-map ,max-let-depth ,(inner body))]
    [(let-one rhs body type unused?)
     `(let-one ,(inner rhs) ,(inner body))]
    [(localref unbox? pos clear? other-clears? type)
     `(localref ,pos ,unbox? ,clear? ,other-clears? ,type)]
    [(mod name srcname self-modidx prefix provides requires body 
          syntax-bodies unexported max-let-depth dummy lang-info 
          internal-context pre-submodules post-submodules)
     `(mod ,name ,(map inner body))]
    [(primval id)
     `(primval ,id)]
    [(seq forms)
     `(seq ,@(map inner forms))]
    [(toplevel depth pos const? ready?)
     `(toplevel ,depth ,pos ,const? ,ready?)]
    [f (error 'init "unknown form ~a" f)]))
  (values (inner e) text-segment))
