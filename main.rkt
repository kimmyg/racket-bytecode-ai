#lang racket/base

(require racket/pretty
         compiler/zo-parse
         "racket-machine/model-impl.rkt")

(for ([file (in-vector (current-command-line-arguments))])
  (with-handlers ([exn:fail? (λ (e)
                               (printf "failed on ~a~n" file)
                               (let ([message (exn-message e)])
                                 (displayln (substring message
                                                       0
                                                       (min (string-length message) 256)))))])
    (pretty-print (impl->model (call-with-input-file file zo-parse)))))
