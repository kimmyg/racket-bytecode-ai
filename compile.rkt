#lang racket/base
(require racket/port
         compiler/zo-parse)

(provide (rename-out [compile* compile]))

(define (compile* p)
  (parameterize ([current-namespace (make-base-namespace)]
                 [compile-context-preservation-enabled #t])
    (let ([a (compile p)]
          [b (open-output-bytes)])
      (write a b)
      (with-input-from-bytes (get-output-bytes b) zo-parse))))
      ;(gc-toplevels (with-input-from-bytes (get-output-bytes b) zo-parse)))))