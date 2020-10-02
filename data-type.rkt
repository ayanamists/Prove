#lang racket

(require (lib "eopl.ss" "eopl"))
(provide (all-defined-out))

(define environment?
  (λ(x) #t))

(define empty-environment
  (lambda (x)
    (eopl:error 'environment "~s is unbound" x)))

(define extend-environment
  (lambda (env var val)
    (lambda (now-var)
      (if (equal? var now-var)
          val
          (env now-var)))))

(define apply-environment
  (lambda (env var)
    (env var)))

(define list->environment
  (lambda (l)
    (define iter
      (lambda (l e)
        (if (null? l)
            e
            (iter
             (cdr l)
             (extend-environment e (car (car l))
                                 (cadr (car l)))))))
    (iter (reverse l) empty-environment)))

(define var-env empty-environment)
(define assert-env empty-environment)

(define (extend-var-env! id val)
  (set! var-env (extend-environment var-env id val)))
(define (extend-assert-env! id val)
  (set! assert-env (extend-environment assert-env id val)))

(define (clear-var-env!)
  (set! var-env empty-environment))
(define (clear-assert-env!)
  (set! var-env empty-environment))

(define (get-var id)
  (apply-environment var-env id))
(define (get-assert id)
  (apply-environment assert-env id))