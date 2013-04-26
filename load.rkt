#lang racket/base
(require racket/list
         racket/match
         compiler/zo-structs
         "extensions.rkt"
         (only-in "stack.rkt" empty-stack)
         (prefix-in heap- (except-in "map.rkt" map))
         (prefix-in text- (except-in "map.rkt" map)))

(provide load)

(define (φ+ φ n)
  (and φ (cons (+ (car φ) n) (cdr φ))))

(define (zip a b)
  (define (zipl a v)
    (if (empty? a)
        empty
        (cons (cons (first a) v) (zipl (rest a) v))))
  (define (zipr v b)
    (if (empty? b)
        empty
        (cons (cons v (first b)) (zipr v (rest b)))))
  (cond
    [(and (list? a) (list? b))
     (map cons a b)]
    [(and (list? a) (not (list? b)))
     (zipl a b)]
    [(and (not (list? a)) (list? b))
     (zipr a b)]
    [else
     (error 'zip "at least one argument must be a pair given ~a and ~a" a b)]))

(define (unzip a×b-s)
  (if (empty? a×b-s)
      (values empty empty)
      (let-values ([(a×b) (first a×b-s)]
                   [(as bs) (unzip (rest a×b-s))])
        (values (cons (car a×b) as)
                (cons (cdr a×b) bs)))))

(define (load e T)
  (let*-values ([(ys es) (text-extract T)]
                [(e*-es* H T) (load′* (zip (cons e es) #f))])
    (values 'uninit
            empty-stack
            H
            (text-concat (text-init ys (rest e*-es*)) T)
            (list (first e*-es*)))))

(define (load′ e φ)
  (match e
    [(and v
          (or (? void?)
              (? number?)
              (? string?)
              (? symbol?)))
     (values v heap-empty text-empty)]
    [(application (localref #f n #f #f #f) e-rs)
     (=> fall-through)
     (let ([m (length e-rs)])
       (if (and φ
                (= n (+ (car φ) m))
                (= (cadr φ) m))
           (let-values ([(e-rs* H T) (load′* (zip e-rs #f))])
             (values (self-app (caddr φ) (localref n #f #f #f #f) e-rs*) H T))
           (fall-through)))]
    [(application e es)
     (let-values ([(e* H-e T-e) (load′ e #f)]
                  [(es* H-es T-es) (load′* (zip es #f))])
       (values (application e* es*) (heap-concat H-e H-es) (text-concat T-e T-es)))]
    [(apply-values e-p e-a)
     (let-values ([(e-p* H-p T-p) (load′ e-p φ)]
                  [(e-a* H-a T-a) (load′ e-a φ)])
       (values (apply-values e-p* e-a*)
               (heap-concat H-p H-a)
               (text-concat T-p T-a)))]
    [(branch e-c e-t e-f)
     (let-values ([(e-c* H-c T-c) (load′ e-c #f)]
                  [(e-t* H-t T-t) (load′ e-t φ)]
                  [(e-f* H-f T-f) (load′ e-f φ)])
       (values (branch e-c* e-t* e-f*)
               (heap-concat H-c H-t H-f)
               (heap-concat T-c T-t T-f)))]
    [(case-lam name es)
     (let-values ([(es* H T) (load′* (zip es #f))])
       (values (case-lam name es*) H T))]
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
     (values e (hasheq) (hasheq) (seteq))]
    [(lam name flags num-params param-types rest? closure-map
           closure-types toplevel-map max-let-depth body)
     (let ([x (fresh)]
           [n (length param-types)])
       (let*-values ([(body* H T) (load′ e #f)])
         (values (lam name flags n rest? closure-map toplevel-map x)
                 H (cons (cons x e*) T))))]
    [(let-one rhs body type unused?)
     (let-values ([(rhs* H-rhs T-rhs) (load′ rhs #f)]
                  [(body* H-body T-body) (load′ body φ)])
       (values (let-one rhs* body* type unused?)
               (heap-concat H-rhs H-body)
               (text-concat T-rhs T-body)))]
    [(and e (localref unbox? pos clear? other-clears? type))
     (values e (hasheq) (hasheq) (seteq))]
    [(mod name srcname self-modidx prefix provides requires body 
          syntax-bodies unexported max-let-depth dummy lang-info 
          internal-context pre-submodules post-submodules)
     (let-values ([(body* H T) (load′* (zip body #f))])
       (values (mod name srcname self-modidx prefix provides requires body*
                    syntax-bodies unexported max-let-depth dummy lang-info 
                    internal-context pre-submodules post-submodules)
               H
               T))]
    [(and e (primval id))
     (values e heap-empty text-empty)]
    [(proc-const name flags num-params param-types rest? toplevel-map max-let-depth body)
     (let ([x (gensym 'addr)]
           [x* (gensym 'addr)])
       (let-values ([(body* H T) (load′ body φ)])
         (values (clos-v x)
                 (heap-set H x (list (clos num-params rest? empty rest? x*)))
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


(define (load-lam-rec e n)
  (match e
    [(lam τs ns e)
     (let ([n (length τs)]
           [x (fresh)])
       (let-values ([(e* H T) (load′ e (cons (length ns) (cons n x)) (set-add x))])
         (values (lam n ns x) H (text-concat (x e*) T))))]
    [l
     (load′ l #f)]))

(define (load-lam-rec* es)
  (if (empty? es)
      (values empty empty empty)
      (let ([l (car (first es))]
            [n (cdr (first es))])
        (let*-values ([(l* H0 T0) (load-lam-rec l n)]
                      [(es* H1 T1) (load-lam-rec* (rest es))])
          (values (cons l* es*) (heap-concat H0 H1) (text-concat T0 T1))))))

