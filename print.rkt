#lang racket/base
(require racket/match
         compiler/zo-structs)

(provide print)

(define print
  (match-lambda
    [(? number? v)
     v]
    [(? string? v)
     v]
    [(? symbol? v)
     v]
    [(application rator rands)
     `(application ,(print rator) ,@(map print rands))]
    [(apply-values proc args-expr)
     `(apply-values ,(print proc) ,(print args-expr))]
    [(branch e0 e1 e2)
     `(branch ,(print e0) ,(print e1) ,(print e2))]
    [(closure code gen-id)
     `(closure ,(print gen-id) ,(print code))]
    [(compilation-top max-let-depth prefix code)
     `(top ,(print prefix) ,(print code))]
    [(def-values ids rhs)
     `(def-values ,(map print ids) ,(print rhs))]
    [(inline-variant direct inline)
     `(inline-variant ,(print direct) ,(print inline))]
    [(lam name flags num-params param-types rest? closure-map
          closure-types toplevel-map max-let-depth body)
     `(λ (,@(map print param-types) . ,(or rest? '())) ,closure-map (,@(map print closure-types)) ,(print body))]
    ;(localref unbox? pos clear? other-clears? type)
    [(let-one e0 e1 type unused?)
     `(let-one ,(print e0) ,(print e1) ,type ,unused?)]
    [(localref #f pos #f other-clears? type)
     `(loc ,pos ,type)]
    [(localref #t pos #f other-clears? type)
     `(loc-box ,pos ,type)]
    [(localref #f pos #t other-clears? type)
     `(loc-clr ,pos ,type)]
    [(localref #t pos #t other-clears? type)
     `(loc-box-clr ,pos ,type)]
    [(mod name srcname self-modidx prefix provides requires body
          syntax-bodies unexported max-let-depth dummy lang-info
          internal-context flags pre-submodules post-submodules)
     `(mod ,@(map print body))]
    [(prefix num-lifts toplevels syntaxes)
     `(prefix ,num-lifts ,toplevels)]
    [(primval id)
     `(primval ,(print id))]
    [(seq es)
     `(seq ,@(map print es))]
    [(toplevel depth pos const? ready?)
     `(toplevel ,depth ,pos)]
    [e (error 'print "can't take ~a" e)]))
