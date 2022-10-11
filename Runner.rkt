#lang racket

(require "Tools.rkt")
(provide (all-defined-out))

; Parsed Neo-Code Runner 
(define run-neo-parsed-code
  (lambda (parsed-code env)
    (cond
      ((null? parsed-code) '())
      ((equal? (car parsed-code) 'num-exp)
       (cadr parsed-code))
      ((equal? (car parsed-code) 'var-exp)
       (resolve env (cadr parsed-code)))
      ((equal? (car parsed-code) 'bool-exp)
       (run-bool-exp (cadr parsed-code)
                     (run-neo-parsed-code (caddr parsed-code) env)
                     (run-neo-parsed-code (cadddr parsed-code) env)))
      ((equal? (car parsed-code) 'math-exp)
       (run-math-exp (cadr parsed-code)
                     (run-neo-parsed-code (caddr parsed-code) env)
                     (run-neo-parsed-code (cadddr parsed-code) env)))
      ((equal? (car parsed-code) 'ask-exp)
       (if (run-neo-parsed-code (cadr parsed-code) env)
           (run-neo-parsed-code (caddr parsed-code) env)
           (run-neo-parsed-code (cadddr parsed-code) env)))
      ((equal? (car parsed-code) 'func-exp)
       (run-neo-parsed-code (cadr (caddr parsed-code)) env))
      (else (run-neo-parsed-code
             (cadr parsed-code)
             (extend-env
              (cadr (cadr (cadr parsed-code)))
              (map (lambda (exp) (run-neo-parsed-code exp env)) (caddr parsed-code))
              env)
             )
            )
      )
    ) 
  )