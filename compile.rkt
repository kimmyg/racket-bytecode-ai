#lang racket/base
(require racket/port
         racket/system
         compiler/zo-parse)

(provide (rename-out [compile* compile])
         compile+demodularize)

(define (compile* p)
  (parameterize ([current-namespace (make-base-namespace)]
                 [compile-context-preservation-enabled #t])
    (let ([a (compile p)]
          [b (open-output-bytes)])
      (write a b)
      (with-input-from-bytes (get-output-bytes b) zo-parse))))

(define (compile+demodularize p)
  (let* ([id (number->string (equal-hash-code p) 16)]
         [file-path (format "tests/~a.rkt" id)]
         [gced-path (format "tests/~a_rkt_merged.zo" id)])
    (with-output-to-file file-path
      (λ () (write p))
      #:exists 'replace)
    (if (system (format "/usr/bin/raco demodularize ~a" file-path))
        (with-input-from-file gced-path zo-parse)
        (error 'compile+demodularize "demodularize call failed"))))
