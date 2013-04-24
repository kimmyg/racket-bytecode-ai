#lang racket/base
(require racket/list
         racket/match
         racket/set)

(provide load)

(define (φ+ φ n)
  (or φ
      (cons (+ (car φ) n) (cdr φ))))

(define (concat h0 . hs)
  (define (concat2 h0 h1)
    (for/fold ([h h0])
      ([(addr value) h1])
      (if (hash-has-key? h addr)
          (if (equal? (hash-ref h addr) value)
              h
              (error 'concat "values for ~a were not the same (~a and ~a)\n" addr (hash-ref h addr) value))
          (hash-set h addr value))))
  (if (empty? hs)
      h0
      (let ([h1 (first hs)]
            [hs (rest hs)])
        (apply concat (concat2 h0 h1) hs))))

(define (zip a b)
  (define (zip* a b)
    (if (or (empty? a)
            (empty? b))
        empty
        (cons (cons (first a)
                    (first b))
              (zip* (rest a) (rest b)))))
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
     (zip* a b)]
    [(and (list? a) (not (list? b)))
     (zipl a b)]
    [(and (not (list? a)) (list? b))
     (zipr a b)]
    [else
     (error 'zip "at least one argument must be a pair, given ~a and ~a" a b)]))

(define (unzip a×b-s)
  (if (empty? a×b-s)
      (values empty empty)
      (let-values ([(a×b) (first a×b-s)]
                   [(as bs) (unzip (rest a×b-s))])
        (values (cons (car a×b) as)
                (cons (cdr a×b) bs)))))

(define (fresh xs [x (gensym 'addr)])
  (if (set-member? xs x)
      (fresh xs)
      x))

(define (load e T)
  (let*-values ([(ys es) (unzip (hash->list T))]
                [(e*-es* H T ys*) (load′* (cons (cons e #f) (zip es #f)) ys)])
    (values 'uninit
            '(((ε)))
            H
            (concat (make-hasheq (zip ys (rest e*-es*))) T)
            `(,(first e*-es*)))))

(define (load′ e φ ys)
  (match e
    ['void
     (values 'void
             empty
             empty
             ys)]
    [(and v
          (or (? number?)
              (? string?)
              (? symbol?)))
     (values v
             empty
             empty
             ys)]
    [`(application (localref ,n #f #f #f #f) ,e-rs ...)
     (=> fall-through)
     (let ([m (length e-rs)])
       (if (and φ
                (= n (+ (car φ) m))
                (= (cadr φ) m))
           (let-values ([(e-rs* H T ys*) (load′* (zip e-rs #f) ys)])
             (values `(self-app ,(caddr φ) (localref ,n #f #f #f #f) ,@e-rs*)
                     H T ys*))
           (fall-through)))]
    [`(application ,es ...)
     (let-values ([(es* H T ys*) (load′* (zip es #f) ys)])
       (values `(application ,es*) H T ys*))]
    [`(apply-values ,e-p ,e-a)
     (let*-values ([(e-p* H-p T-p ys*) (load′ e-p φ ys)]
                   [(e-a* H-a T-a ys**) (load′ e-a φ ys*)])
       (values `(apply-values ,e-p* ,e-a*)
               (concat H-p H-a) (concat T-p T-a) ys**))]
    [`(branch ,e-c ,e-t ,e-f)
     (let*-values ([(e-c* H-c T-c ys*) (load′ e-c #f ys)]
                   [(e-t* H-t T-t ys**) (load′ e-t φ ys*)]
                   [(e-f* H-f T-f ys***) (load′ e-f φ ys**)])
       (values `(branch ,e-c* ,e-t*, e-f*) (concat H-c H-t H-f) (concat T-c T-t T-f) ys***))]
    [`(case-lam ,name ,es)
     (let-values ([(es* H T ys*) (load′* (zip es #f) ys)])
       (values `(case-lam ,name ,es*) H T ys*))]
    #;[(and e (closure ,code ,gen-id))
     (let ([proc (cons 'proc-const (rest (inner code)))])
       (if (set-member? cycle-points e)
           `(indirect ,(or (hash-ref clos-address e #f)
                           (let ([addr (genaddr)])
                             (hash-set! clos-address e addr)
                             (hash-set! text-segment addr proc)
                             addr)))
           proc))]
    [`(compilation-top ,max-let-depth ,prefix ,e)
     (let-values ([(e* H T ys*) (load′ e #f ys)])
       (values `(compilation-top ,max-let-depth ,e*) H T ys*))]
    [`(def-values ,ids ,e)
     (let-values ([(e* H T ys*) (load′ e #f ys)])
       (values `(def-values ,ids ,e*) H T ys*))]
    [(and e `(indirect ,x))
     (values e (hasheq) (hasheq) (seteq))]
    [`(lam ,name ,flags ,param-types ,rest? ,closure-map
           ,closure-types ,toplevel-map ,max-let-depth ,e)
     (let ([x (fresh ys)]
           [n (length param-types)])
       (let*-values ([(e* H T ys*) (load′ e #f (cons x ys))])
         (values `(lam ,name ,flags ,n ,rest? ,closure-map ,toplevel-map ,x)
                 H (cons (cons x e*) T) ys*)))]
    [`(let-one ,e-r ,e-b)
     (let*-values ([(e-r* H-e-r T-e-r ys*) (load′ e-r #f ys)]
                   [(e-b* H-e-b T-e-b ys**) (load′ e-b φ ys*)])
       (values `(let-one ,e-r ,e-b) (concat H-e-r H-e-b) (concat T-e-r T-e-b) ys**))]
    [(and e `(localref ,unbox? ,pos ,clear? ,other-clears? ,type))
     (values e (hasheq) (hasheq) (seteq))]
    [`(mod ,name ,es)
     (let-values ([(es* H T ys*) (load′* (zip es #f) ys)])
       (values `(mod ,name ,es*) H T ys*))]
    [(and e `(primval ,id))
     (values e (hasheq) (hasheq) (seteq))]
    [`(proc-const ,name ,flags ,param-types ,rest? ,closure-map ,closure-types
                  ,toplevel-map ,max-let-depth ,e)
     (let-values ([(e* H T ys*) (load′ e φ ys)])
       (values `(proc-const ,name ,flags ,param-types ,rest? ,closure-map ,closure-types
                            ,toplevel-map ,max-let-depth ,e*)
               H T ys*))]
    [`(seq ,es ... ,e)
     (let*-values ([(es* Hs Ts ys*) (load′* (zip es #f) ys)]
                   [(e* H T ys**) (load′ e φ ys)])
       (values `(seq ,@es* ,e*) (concat Hs H) (concat Ts T) ys**))]
    [(and e `(toplevel ,depth ,pos ,const? ,ready?))
     (values e (hasheq) (hasheq) (seteq))]))

(define (load′* es ys)
  (if (empty? es)
      (values empty empty empty ys)
      (let ([e (car (first es))]
            [φ (cdr (first es))])
        (let*-values ([(e* H0 T0 ys*) (load′ e φ ys)]
                      [(es* H1 T1 ys**) (load′* (rest es) ys*)])
          (values (cons e* es*) (concat H0 H1) (concat T0 T1) ys**)))))


(define (load-lam-rec e n ys)
  (match e
    [`(lam ,τs ,ns ,e)
     (let ([n (length τs)]
           [x (fresh ys)])
       (let-values ([(e* H T ys*) (load′ e (cons (length ns) (cons n x)) (set-add ys x))])
         (values `(lam ,n ,ns ,x) H (concat (x e*) T) ys*)))]
    [l
     (load′ l #f ys)]))

(define (load-lam-rec* es ys)
  (if (empty? es)
      (values empty empty empty ys)
      (let ([l (car (first es))]
            [n (cdr (first es))])
        (let*-values ([(l* H0 T0 ys*) (load-lam-rec l n ys)]
                      [(es* H1 T1 ys**) (load-lam-rec* (rest es) ys*)])
          (values (cons l* es*) (concat H0 H1) (concat T0 T1) ys**)))))

