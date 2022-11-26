#lang racket
(provide parse)
(require syntax/parse
         "term.rkt")

(define (parse stx)
  (syntax-parse stx
    #:datum-literals (U
                      lam
                      Pi :
                      ->
                      let = in)
    [U (Univ)]
    [(lam x t) (Lam (syntax->datum #'x) (parse #'t))]
    [(let x : a = t in u) (Let (syntax->datum #'x) (parse #'a) (parse #'t) (parse #'u))]
    [((x : a) -> b) (Pi (syntax->datum #'x) (parse #'a) (parse #'b))]
    [(a -> b) (Pi '_ (parse #'a) (parse #'b))]
    [(t u) (App (parse #'t) (parse #'u))]
    [x:id (Var (syntax->datum #'x))]))
