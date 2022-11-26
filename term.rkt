#lang typed/racket/optional
(provide (all-defined-out))
(define-type Name Symbol)

(struct Univ () #:transparent)
(struct Var ([x : Name]) #:transparent)
(struct App ([f : Tm] [arg : Tm]) #:transparent)
(struct Lam ([x : Name] [body : Tm]) #:transparent)
(struct Pi ([x : Name] [x-ty : Ty] [body : Ty]) #:transparent)
(struct Let ([x : Name] [a : Ty] [t : Tm] [body : Tm]) #:transparent)
(define-type Tm (U Univ Var App Lam Pi Let))
(define-type Ty Tm)
