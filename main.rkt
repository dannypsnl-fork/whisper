#lang racket
(require "parse.rkt"
         "core.rkt")

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "whisper"
   #:args ()
   ; let Nat : U = (N : U) -> (N -> N) -> N -> N;
   (define tm #'(let id : ((A : U) -> (A -> A)) = (lam A (lam x x))
                  in (id U)))
   (define ty (infer-empty (parse tm)))
   (printf "type of term ~a is ~a~n" tm (readback '() ty))))
