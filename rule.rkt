#lang racket

(require (lib "eopl.ss" "eopl"))
(require "lang.rkt")
(require "right-expr.rkt")
(require "data-type.rkt")
(provide (all-defined-out))

(define (pmatch-error p v)
  (eopl:error 'pmatch-error
              "~a don't match ~a" v p))

(define (pmatch p v)
  ;(printf "~a ~a ~n" p v)
  (cases right-expression v
    [b-rexp (exp) (pmatch p exp)]
    [else
     (cases right-expression p
       [and-rexp
        (pexp1 pexp2)
        (cases right-expression v
          [and-rexp
           (vexp1 vexp2)
           (append (pmatch pexp1 vexp1)
                   (pmatch pexp2 vexp2))]
          [else (pmatch-error p v)])]
       [or-rexp
        (pexp1 pexp2)
        (cases right-expression v
          [or-rexp
           (vexp1 vexp2)
           (append (pmatch pexp1 vexp1)
                (pmatch pexp2 vexp2))]
          [else (pmatch-error p v)])]
       [->-rexp
        (pexp1 pexp2)
        (cases right-expression v
          [->-rexp
           (vexp1 vexp2)
           (append (pmatch pexp1 vexp1)
                   (pmatch pexp2 vexp2))]
          [else (pmatch-error p v)])]
       [not-rexp
        (pexp1)
        (cases right-expression v
          [not-rexp (vexp1)
                    (pmatch pexp1 vexp1)]
          [else (pmatch-error p v)])]
       [id-rexp (id)
                (list (list id v))]
       [b-rexp (pexp1)
            (pmatch pexp1 v)]
       [else (eopl:error 'pmatch
                         "~a is not legal pattern"
                         p)])]))

(define (merge l1 l2)
  (if (null? l1)
      l2
      (let* ([this (car l1)]
             [name (car this)])
        (if (boolean?
             (memf (λ (x) (equal? name (car x)))
                   l2))
            (merge (cdr l1) (cons this l2))
            (merge (cdr l1) l2)))))

(define (pmatch-env p l)
  (if (null? p)
      (begin 
        ;(printf "~a" l)
        (list->environment l))
      (let* ([this (car p)]
             [name (car this)]
             [pattern (cdr this)]
             [this-pair (findf (λ (x) (equal? name (car x))) l)]
             [this-v (cadr this-pair)])
        (pmatch-env (cdr p)
                    (merge (pmatch pattern this-v)
                            l)))))
                

(define (handle-ante p v l)
  (cond
    [(null? p)
      (list->environment l)]
    [(null? v)
     (pmatch-env p l)]
    [else
     (let* ([this (car p)]
            [name (car this)]
            [pattern (cdr this)]
            [this-v (car v)])
       (let ([match-res (pmatch pattern this-v)])
         (handle-ante (cdr p) (cdr v) (merge l match-res))))]))

(define (make-rule ante-pattern legal-conclusion)
  (λ (ante real-conclusion)
    (let ([env (handle-ante ante-pattern ante '())])
      (let ([target-conclusion (eval-rexpr legal-conclusion env)])
        (if (rexpr-equal? target-conclusion real-conclusion empty-environment)
            #t
            (eopl:error 'make-rule "~a is not ~a" real-conclusion target-conclusion)
            )))))
     
(define (apply-rule rule antes conclusion)
  (rule antes conclusion))     
       
        
            
        
         
         
         
              
    