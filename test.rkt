#lang racket

(require "prove.rkt")
(require "lang.rkt")
(require racket/trace)

(define (test path)
  (eval-program
   (scan&parse
    (file->string path))))

(test "rules/basic-logic.prv")
              