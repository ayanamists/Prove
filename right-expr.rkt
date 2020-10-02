#lang racket

(require "lang.rkt")
(require "data-type.rkt")
(require (lib "eopl.ss" "eopl"))
(provide (all-defined-out))

(define (eval-rexpr expr env)
  (cases right-expression expr
    [b-rexp (exp) (eval-rexpr exp env)]
    [id-rexp (id) (apply-environment env id)]
    [and-rexp (exp1 exp2) (and-rexp (eval-rexpr exp1 env)
                                    (eval-rexpr exp2 env))]
    [or-rexp (exp1 exp2) (or-rexp (eval-rexpr exp1 env)
                                  (eval-rexpr exp2 env))]
    [->-rexp (exp1 exp2) (->-rexp (eval-rexpr exp1 env)
                                  (eval-rexpr exp2 env))]
    [not-rexp (exp1) (not-rexp (eval-rexpr exp1 env))]
    [else expr]
    ))

(define (eval-rexprs env lexpr)
  (map (λ (x) (eval-rexpr x env)) lexpr))

(define (rexpr-equal? p v env)
  (cases right-expression v
    [id-rexp (id) (rexpr-equal? p (apply-environment env id) env)]
    [b-rexp (v) (rexpr-equal? p v env)]
    [else 
     (cases right-expression p
       [and-rexp
        (pexp1 pexp2)
        (cases right-expression v
          [and-rexp
           (vexp1 vexp2)
        (or (and (rexpr-equal? pexp1 vexp2 env)
                 (rexpr-equal? pexp2 vexp1 env))
            (and (rexpr-equal? pexp1 vexp1 env)
                 (rexpr-equal? pexp2 vexp2 env)))]
          [else #f])]
       
       [or-rexp
        (pexp1 pexp2)
        (cases right-expression v
          [or-rexp
           (vexp1 vexp2)
            (or (and (rexpr-equal? pexp1 vexp2 env)
                     (rexpr-equal? pexp2 vexp1 env))
                (and (rexpr-equal? pexp1 vexp1 env)
                     (rexpr-equal? pexp2 vexp2 env)))]
          [else #f])]
       
       [->-rexp
        (pexp1 pexp2)
        (cases right-expression v
          [->-rexp
           (vexp1 vexp2)
           (and (rexpr-equal? pexp1 vexp1 env)
                 (rexpr-equal? pexp2 vexp2 env))]
          [else #f])]
       
       [not-rexp
        (pexp1)
        (cases right-expression v
          [not-rexp (vexp1)
                    (rexpr-equal? pexp1 vexp1 env)]
          [else #f])]
       
       [id-rexp (id)
                (rexpr-equal? (apply-environment env id)
                              v env)]
       
       [bool-rexp
        (val1)
        (cases right-expression v
          [bool-rexp (val2)
                     (= val1 val2)]
          [else #f])]

       [b-rexp (exp1) (rexpr-equal? exp1 v env )]
       [sym-rexp (sym)
                 (cases right-expression v
                   [sym-rexp (sym2)
                             (equal? sym sym2)]
                   [else #f])]
       )]))