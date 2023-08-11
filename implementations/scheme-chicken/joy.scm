(import (chicken io))
(import (chicken string))
(import srfi-69)


(define (joy stack expression dict)
  (if (null? expression)
    (values stack dict)
    (if (string? (car expression))
      (receive (s e d)
        (joy-eval (car expression) stack (cdr expression) dict)
        (joy s e d))
      (joy (cons (car expression) stack) (cdr expression) dict))))

(define (joy-eval symbol stack expression dict)
  (define (is-it? name) (string=? symbol name))
  (cond
    ((is-it? "+") (values (joy-add stack) expression dict))
    ((is-it? "-") (values (joy-sub stack) expression dict))
    ((is-it? "dup") (values (joy-dup stack) expression dict))
    (else (values (cons symbol stack) expression dict))))

(define (joy-add stack) (cons (+ (cadr stack) (car stack)) (cddr stack)))
(define (joy-sub stack) (cons (- (cadr stack) (car stack)) (cddr stack)))
(define (joy-dup stack) (cons (car stack) stack))


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

(define (text->expression text) (parse (tokenize text)))


(define (joy-term->string term)
  (cond ((boolean? term) (if term "true" "false"))
        ((number? term) (->string term))
        ((list? term) (conc "[" (joy-expression->string term) "]"))
        (else term)))

(define (joy-expression->string expr)
  (string-intersperse (map joy-term->string expr) " "))

(define (doit text)
  (receive (stack _dict)
    (joy '() (text->expression text) (initialize))
    (joy-expression->string stack)))


(define (initialize)
  (load-defs
    ; TODO: load defs at compile-time, not run-time.
    (with-input-from-file "../defs.txt" read-string)
    (make-hash-table string=? string-hash)))

(define (load-defs defs dict)
  (map (lambda (def) (add-def def dict)) (string-split defs "\n"))
  dict)

(define (add-def def dict)
  (let ((def_list (text->expression def)))
    (hash-table-set! dict (car def_list) (cdr def_list))))


(display (doit "ab  cd [[  ]] 23 4 - dup - [true] false"))
(newline)

