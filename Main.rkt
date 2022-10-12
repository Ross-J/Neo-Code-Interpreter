#lang Racket

(require "Parser.rkt")
(require "Runner.rkt")
(require "Tools.rkt")
(require "Variable_Env.rkt")



(define env '((a 1) (b 2) (c 5)))

(define sample-code '(call (function () (ask (bool > a b) (math - a b) (math + a b))) (a)))
(display (neo-parser sample-code))
(define parsed-neo-code (neo-parser sample-code))
(run-neo-parsed-code parsed-neo-code env)
