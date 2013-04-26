#lang racket/base
(require racket/list
         racket/match
         compiler/zo-structs
         "extensions.rkt"
         "stack.rkt"
         (prefix-in heap- "map.rkt")
         (prefix-in text- "map.rkt")
         "primitive-maps.rkt")

(provide step)

(define (alloc n S)
  S
  #;(if (zero? n)
        S
        (cons (make-vector n) S)))

(define prefix->buckets
  (match-lambda
    [(prefix num-lifts toplevels syntaxes)
     (define num-toplevels (length toplevels))
     (define num-syntaxes (length syntaxes))
     (define buckets (make-vector (+ num-toplevels
                                     num-syntaxes
                                     num-lifts)))
     (for ([toplevel toplevels]
           [i num-toplevels])
       (vector-set! buckets i toplevel))
     (for ([syntax syntaxes]
           [i num-syntaxes])
       (vector-set! buckets (+ num-toplevels i) syntax))
     buckets]))

(define eval-toplevel
  (match-lambda
    [(module-variable modidx sym pos phase constantness)
     (dynamic-require (module-path-index-resolve modidx) sym)]
    [e e]))

(define (V->vs V)
  (if (and (list? V)
           (not (empty? V))
           (not (symbol? (first V)))) ; XXX huge hack remove with structs
      V
      (list V)))

(define (step V S H T C)
  (if (empty? C)
      (error 'step "done ~a" V)
      (let ([c (first C)]
            [C (rest C)])
        (match c
          [(and v (or (? number?)))
           (values v S H T C)]
          [(application rator rands)
           (stack-push (length rands))]
          [(apply-values proc args-expr)
           (values V (stack-push-uninit S 1) H T (list* 'framepush proc 'framepop (set 0) 'framepush args-expr 'framepop 'apply-values C))]
          ['apply-values ; XXX hack
           (let ([vs (V->vs V)])
             (values (stack-ref S 0) (stack-push (stack-pop S 1) vs) H T (list* (call (length vs)) C)))]
          [(call n) ; XXX hack
           (match V
             [(clos-v x)
              (match-let ([(clos _ vs _ x′)
                           (findf
                            (match-lambda
                              [(clos n′ _ rest? _)
                               (or (= n′ n) (and rest? (< n′ n)))]
                              [c (error 'step "not closure ~a" c)])
                            (hash-ref H x))])
                (values V (append vs S) H T (cons (hash-ref T x′) C)))]
             [_ (error "not a procedure ~a" V)])]
          [(compilation-top max-let-depth prefix e)
           (values V (alloc max-let-depth (cons (prefix->buckets prefix) S)) H T (cons e C))]
          [(and v (clos-v x))
           (values v S H T C)]
          [`(def-ids ,ids) ; XXX hack
           (let ([vs (V->vs V)])
             (unless (= (length ids) (length vs))
               (error 'step "def-values expected ~a values got ~a" (length ids) (length vs)))
             (map
              (λ (id v)
                (match id
                  [(toplevel depth pos _ _)
                   (vector-set! (list-ref S depth) pos v)]
                  [_
                   (error 'step "def-values unknown id ~a" id)]))
              ids vs)
             (values V S H T C))]
          [(def-values ids rhs)
           (values V S H T (list* (framepush) rhs (framepop)  `(def-ids ,ids) C))]
          ['framepush
           (values V (stack-shift S) H T C)]
          ['framepop
           (values V (stack-reset S) H T C)]
          [(indirect x)
           (values V S H T (list* (text-ref T x) C))]
          [(let-one rhs body type unused?)
           (values V (stack-push-uninit S 1) H T (list* 'framepush rhs 'framepop (set 0) body C))]
          [(localref #f n #f _ #f)
           (values (stack-ref S n) S H T C)]
          [(localref #f n #t _ #f)
           (values (stack-ref S n) (stack-clear S n) H T C)]
          [(localref #t n #f _ #f)
           (values (heap-ref H (stack-ref S n)) S H T C)]
          [(localref #t n #t _ #f)
           (values (heap-ref H (stack-ref S n)) (stack-clear S n) H T C)]
          [(mod name srcname self-modidx prefix provides requires body 
                syntax-bodies unexported max-let-depth dummy lang-info 
                internal-context pre-submodules post-submodules)
           (values V (stack-push S (prefix->buckets prefix)) H T (append body C))]
          ['(push)
           (values V (cons V S) H T C)]
          [(seq (list e0 e1))
           (values V S H T (list* (framepush) e0 (framepop) e1 C))]
          [(seq (cons e0 es))
           (values V S H T (list* (framepush) e0 (framepop) (seq es) C))]
          [(toplevel depth pos const? eady?)
           (values (eval-toplevel (vector-ref (list-ref S depth) pos)) S H T C)]
          
          [c (error 'step "unrecognized form ~a" c)]))))

