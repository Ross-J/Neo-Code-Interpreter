#lang racket

(provide (all-defined-out))

; Tool to resolve a value from the environment
(define resolve
  (lambda (environment varname)
    (cond
      ((null? environment) #false)
      ((equal? (caar environment) varname) (cadar environment))
      (else (resolve (cdr environment) varname))
      )
    )
  )

; Tool to extend variable environment
(define extend-env
  (lambda (list-of-varname list-of-value env) ;((x y z) (1 2 3) env)
    (cond
      ((null? list-of-varname) env)
      ((null? list-of-value) env)
      (else (extend-env (cdr list-of-varname) (cdr list-of-value)
       (cons (list (car list-of-varname)
                   (car list-of-value))
             env)))
      )
    )
  )

; Math Expression Tool
(define run-math-exp
  (lambda (op num1 num2)
    (cond
      ((equal? op '+) (+ num1 num2)) 
      ((equal? op '-) (- num1 num2)) 
      ((equal? op '*) (* num1 num2)) 
      ((equal? op '/) (/ num1 num2))  
      ((equal? op '//) (quotient num1 num2)) 
      ((equal? op '%) (modulo num1 num2)) 
      (else #false)
      )
    )
  )


; Boolean Expression Tool
(define run-bool-exp
  (lambda (op num1 num2)
    (cond
      ((equal? op '>) (> num1 num2))
      ((equal? op '<) (< num1 num2))
      ((equal? op '>=) (>= num1 num2))
      ((equal? op '<=) (<= num1 num2))
      ((equal? op '==) (= num1 num2))
      ((equal? op '!=) (not (= num1 num2)))
      ((equal? op '&&) (and num1 num2))
      ((equal? op '||) (or num1 num2))
      (else (not num1))
      )
    )
  )