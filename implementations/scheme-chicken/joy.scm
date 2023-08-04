(import (chicken string))

(define (joy stack expression dict)
  (if (null? expression)
    (values stack dict)
    (if (string? (car expression))
      (receive (s e dict0)
        (joy-eval (car expression) stack (cdr expression) dict)
        (joy s e dict0))
      (joy (cons (car expression) stack) (cdr expression) dict))))

(define (joy-eval symbol stack expression dict)
  (define (is-it? name) (string=? symbol name))
  (cond
    ((is-it? "+") (values (joy-add stack) expression dict))
    ((is-it? "-") (values (joy-sub stack) expression dict))
    (else (values (cons symbol stack) expression dict))))

(define (joy-add stack) (cons (+ (cadr stack) (car stack)) (cddr stack)))
(define (joy-sub stack) (cons (- (cadr stack) (car stack)) (cddr stack)))


(define (string-replace str from to)
  (string-intersperse (string-split str from #t) to))

(define (tokenize str)
  (string-split
    (string-replace (string-replace str "]" " ] ") "[" " [ ")))

(define (tokenator token)
  (cond ((string->number token) (string->number token))
        ((string=? token "true") #t)
        ((string=? token "false") #f)
        (else token)))

(define (expect-right-bracket tokens acc) 
  (if (null? tokens)
    (error "Missing closing bracket.")
    (expect-right-bracket-lookahead (car tokens) (cdr tokens) acc)))

(define (expect-right-bracket-lookahead token tokens acc)
  (cond ((string=? token "]") (values acc tokens))
        ((string=? token "[")
          (receive (sub_list rest) (expect-right-bracket tokens '())
            (receive (el rrest) (expect-right-bracket rest acc)
              (values (cons sub_list el) rrest))))
        (else 
          (receive (el rest) (expect-right-bracket tokens acc)
            (values (cons (tokenator token) el) rest)))))

(define (one-token-lookahead token tokens)
  (cond ((string=? token "]") (error "Extra closing bracket."))
        ((string=? token "[") (expect-right-bracket tokens '()))
        (else (values (tokenator token) tokens))))

(define (parse0 tokens acc)
  (if (null? tokens)
    acc
    (receive (term rest_of_tokens)
      (one-token-lookahead (car tokens) (cdr tokens))
      (cons term (parse0 rest_of_tokens acc)))))

(define (parse tokens) (parse0 tokens '()))

(define (text-to-expression text) (parse (tokenize text)))


(define (joy-term-to-string term)
  (cond ((boolean? term) (if term "true" "false"))
        ((number? term) (->string term))
        ((list? term) (conc "[" (joy-expression-to-string term) "]"))
        (else term)))

(define (joy-expression-to-string expr)
  (string-intersperse (map joy-term-to-string expr) " "))

(define (doit text)
  (receive (stack dict)
    (joy '() (text-to-expression text) '())
    (joy-expression-to-string stack)))

(display (doit "ab  cd [[  ]] 23 4 - [true] false"))
(newline)

