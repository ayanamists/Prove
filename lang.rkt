(module lang

  ;; grammar for the LET language

  (lib "eopl.ss" "eopl")                
  
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       ((or "_" letter) (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (sym ("'" (arbno (or letter digit "_" "-" "?"))) symbol)
      ))
  
  (define the-grammar
    '((program (expression (arbno ";" expression)) a-program)
      
      (expression ("let" "(" identifier "," right-expression ")") bind-exp)
      (expression ("with" "(" identifier (arbno "," identifier) ")") with-exp)
      
      (expression ("assert"
                   identifier
                   "by" identifier (arbno "," identifier)
                   "using" identifier
                   ) assert-exp)
      
      (expression ("define-rules"
                   "name:" identifier
                   "antes:" identifier (arbno "," identifier)
                   "contact:" contact-expression (arbno "," contact-expression)
                   "conclusion:" right-expression) dr-exp)
      
      (contact-expression (identifier "is" right-expression) c-cexp)
      
      (right-expression ("(" right-expression ")") b-rexp)
      (right-expression (number) bool-rexp)
      (right-expression (identifier) id-rexp)
      (right-expression ("or" right-expression right-expression) or-rexp)
      (right-expression ("and" right-expression right-expression) and-rexp)
      (right-expression ("not" right-expression) not-rexp)
      (right-expression ("->" right-expression right-expression) ->-rexp)
      (right-expression (sym) sym-rexp)
      
      ))
  
  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
