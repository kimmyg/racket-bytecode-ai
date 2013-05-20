#lang racket/base
(require racket/list
         racket/match
         compiler/zo-structs
         "extensions.rkt"
         "stack.rkt"
         (prefix-in heap- "map.rkt")
         (prefix-in text- "map.rkt")
         "primitive-maps.rkt"
         "zip.rkt")

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
        ;(displayln c)
        (match c
          [(and v (or (? number?)))
           (values v S H T C)]
          [(application rator rands)
           (let ([n (length rands)])
             (values V
                     (stack-push-uninit S n)
                     H
                     T
                     (cons (reorder (call n) (cons (cons rator '?) (zip rands (build-list n values)))) C)))]
          [(apply-values proc args-expr)
           (values V S H T (list* (framepush) proc (framepop) (let-void 1 #f (set 0)) (framepush) args-expr (framepop) 'apply-values C))]
          ['apply-values ; XXX hack
           (values (stack-ref S 0) (apply stack-push (stack-pop S 1) V) H T (list* (call (length V)) C))]
          [(call n) ; XXX hack
           (match V
             [(clos-v x)
              (match-let ([(clos _ _ vs x′)
                           (findf
                            (match-lambda
                              [(clos n′ rest? _ _)
                               (or (= n′ n) (and rest? (< n′ n)))]
                              [c (error 'step "not closure ~a" c)])
                            (heap-ref H x))])
                (values V (apply stack-push S vs) H T (cons (text-ref T x′) C)))]
             [(? procedure? p)
              (values (call-with-values (λ () (apply p (stack-take S n))) list) S H T C)]
             [_ (error 'step/call "not a procedure ~a" V)])]
          [(compilation-top max-let-depth prefix e)
           (values V (stack-push S (prefix->buckets prefix)) H T (cons e C))]
          [(and v (clos-v x))
           (values v S H T C)]
          [`(def-ids ,ids) ; XXX hack
           (let ([vs (V->vs V)])
             (unless (= (length ids) (length vs))
               (error 'step "def-values expected ~a values; got ~a" (length ids) (length vs)))
             (map
              (λ (id v)
                (match id
                  [(toplevel depth pos _ _)
                   (vector-set! (stack-ref S depth) pos v)]
                  [_
                   (error 'step "def-values unknown id ~a" id)]))
              ids vs)
             (values V S H T C))]
          [(def-values ids rhs)
           (values V S H T (list* (framepush) rhs (framepop) `(def-ids ,ids) C))]
          [(framepush)
           (values V (stack-shift S) H T C)]
          [(framepop)
           (values V (stack-reset S) H T C)]
          [(indirect x)
           (values V S H T (list* (text-ref T x) C))]
          [(let-one rhs body type unused?)
           (values V (stack-push-uninit S 1) H T (list* (framepush) rhs (framepop) (set 0) body C))]
          [(let-void count boxes? body)
           (if boxes?
               (error 'step "let-void implement for boxes")
               (values V (stack-push-uninit S count) H T (cons body C)))]
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
                internal-context flags pre-submodules post-submodules)
           (values V (stack-push S (prefix->buckets prefix)) H T (append body C))]
          [(primval id)
           (values (primitive-id->value id) S H T C)]
          [`(push ,n)
           (values V (stack-push-uninit S n) H T C)]
          [(reorder (call n) assignments)
           (match assignments
             [(list (cons proc '?))
              (values V S H T (list* (framepush) proc (framepop) (call n) C))]
             [(list (cons proc '?) (and args (cons arg m)) ... (cons arg-last m-last))
              (values V S H T
                      (list* (framepush)
                             proc
                             (framepop)
                             (set m-last)
                             (for/fold ([C (list* (framepush)
                                                  arg-last
                                                  (framepop)
                                                  (swap m-last)
                                                  (call n)
                                                  C)])
                               ([arg (reverse args)])
                               (list* (framepush)
                                      (car arg)
                                      (framepop)
                                      (set (cdr arg))
                                      C))))])]
          [(seq (list e0 e1))
           (values V S H T (list* (framepush) e0 (framepop) e1 C))]
          [(seq (cons e0 es))
           (values V S H T (list* (framepush) e0 (framepop) (seq es) C))]
          [(set n)
           (values V (stack-set S V n) H T C)]
          [(swap n)
           (values (stack-ref S n) (stack-set S V n) H T C)]
          [(toplevel depth pos const? eady?)
           (values (eval-toplevel (vector-ref (stack-ref S depth) pos)) S H T C)]
          
          [c (error 'step "unrecognized form ~a" c)]))))

