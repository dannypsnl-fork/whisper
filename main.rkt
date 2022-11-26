#lang racket
(require "parse.rkt"
         "core.rkt")

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "whisper"
   #:args ()
   ;   let Nat  : U = (N : U) -> (N -> N) -> N -> N;
   (define ty (infer-empty (parse #'(let Nat : U = U
                                      in Nat))))
   (printf "type = ~a~n" (readback '() ty))))
