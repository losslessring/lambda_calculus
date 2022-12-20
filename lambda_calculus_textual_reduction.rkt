#lang racket
(define (expr? e)
  (match e
    [(? symbol? x) #t]
    [`(lambda (,(? symbol? x)) ,(? expr? e)) #t]
    [`(,(? expr? e0) ,(? expr? e1)) #t]
    [_ #f]))



(define (textual-reduction symbol-to-change expression-to-change-with source-expression)
  (displayln "List textual reduction")
  (display "Source lambda body: ")
  (print source-expression)
  (display "   Symbol to change: ")
  (print symbol-to-change)
  (display "   Expression to change with: ")
  (print expression-to-change-with)
  (displayln "")
  (display "Reduction step result: ")
  (println (replace symbol-to-change expression-to-change-with source-expression))
  (replace symbol-to-change expression-to-change-with source-expression))


(define (one-symbol-textual-reduction symbol-to-change expression-to-change-with source-expression)
  (displayln "One symbol textual reduction")
  (display "Source lambda body: ")
  (print source-expression)
  (display "   Symbol to change: ")
  (print symbol-to-change)
  (display "   Expression to change with: ")
  (print expression-to-change-with)
  (displayln "")
  (display "Reduction step result: ")
  (println expression-to-change-with)
  expression-to-change-with)

(define (step e)
  (match e    
    [`((lambda (,(? symbol? x)) ,(? symbol? e0)) ,(? expr? e1)) (one-symbol-textual-reduction x e1 e0)]
    [`((lambda (,(? symbol? x)) ,(? expr? e0)) ,(? expr? e1)) (textual-reduction x e1 e0)]
    [`(,(? expr? e0) ,(? expr? e1)) (cons (step e0) `(,e1))]    
    [_ (error (format "not sure how to step ~a" e))]))

(define (deep-eval e)
  (match e
    [(? rational? n) n]
    [`(lambda (,(? symbol? symbol)) ,(? expr? e0)) `(lambda (,symbol) ,e0)]
    [(? list? e) (deep-eval (step e))]))

(define (replace str rep lst)
  (map (lambda (x) (if (equal? x str) rep x)) 
       lst))




;(expr? '(lambda (x) x))
;(expr? '((lambda (x) x) (lambda (x) x)))
;(step '((lambda (x) x) (lambda (y) y)))
;(step '(((lambda (x) x) (lambda (y) y)) (lambda (z) z)))
;(step (step '(((lambda (x) x) (lambda (y) y)) (lambda (z) z))))

;(deep-eval '((lambda (x) x) (lambda (y) y)))
;(deep-eval '(((lambda (x) x) (lambda (y) y)) ((lambda (z) z) (lambda (a) a))))
;(expr? '((lambda (z) z) (lambda (a) a)))
;(step '((lambda (z) z) (lambda (a) a)))
;(step '((lambda (y) y) ((lambda (z) z) (lambda (a) a))))
;(step (step (step '(((lambda (x) x) (lambda (y) y)) ((lambda (z) z) (lambda (a) a))))))
;(step (step '((lambda (x) (x x)) (lambda (y) (y y)))))
;(step '((lambda (x) x) (lambda (y) y)))
;(deep-eval '((lambda (x) (x x)) (lambda (y) (y y))))