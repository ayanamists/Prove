#lang racket

(require "lang.rkt")
(require "data-type.rkt")
(require "rule.rkt")
(require "right-expr.rkt")
(require (lib "eopl.ss" "eopl"))

(provide eval-program)
 
(define (eval-program prg)
  (cases program prg
    [a-program
     (expr rest-expr)
     (eval-exprs (cons expr rest-expr))]))

(define (set-all-assert! vars)
  (for-each (λ (x) extend-assert-env! x) vars))

(define (contact-exprs->contacts exprs)
  (map
   (λ (x) (cases contact-expression x
            (c-cexp
             (id rexp)
             (if (equal? rexp (id-rexp '_))
                 (cons id (id-rexp id))
                 (cons id rexp)))))
   exprs))

(define (eval-expr expr)
  (cases expression expr
    [bind-exp (id re)
              (extend-var-env! id (eval-rexpr re var-env))]
    
    [with-exp (id rest-id)
      (apply set-all-assert! (cons id rest-id))]
    
    [assert-exp
     (conclusion antecedent rest-antecedent using-rule)
     (let ((antes (map (λ (x) (get-var x)) (cons antecedent rest-antecedent)))
           (rule-value (get-var using-rule))
           (conclusion-val (get-var conclusion)))
       (let ((result (apply-rule rule-value antes conclusion-val)))
         (if result
             (extend-assert-env! conclusion #t)
             #f)))]

    [dr-exp
     (name ante rest-ante contact rest-contact conclusion)
     (let ((contacts (contact-exprs->contacts (cons contact
                                                    rest-contact))))
       (let ((rules (make-rule contacts conclusion)))
         (extend-var-env! name rules)))]
    )) 

(define (eval-exprs exprs)
  (if (null? exprs)
      "proof correct!"
      (begin
        (eval-expr (car exprs))
        (eval-exprs (cdr exprs)))))
        
              
              