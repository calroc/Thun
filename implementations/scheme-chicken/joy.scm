;
;████████╗██╗  ██╗██╗   ██╗███╗   ██╗
;╚══██╔══╝██║  ██║██║   ██║████╗  ██║
;   ██║   ███████║██║   ██║██╔██╗ ██║
;   ██║   ██╔══██║██║   ██║██║╚██╗██║
;   ██║   ██║  ██║╚██████╔╝██║ ╚████║
;   ╚═╝   ╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═══╝
;
;Copyright © 2023 Simon Forman
;
;This file is part of Thun
;
;Thun is free software: you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation, either version 3 of the License, or
;(at your option) any later version.
;
;Thun is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with Thun.  If not see <http://www.gnu.org/licenses/>.
;

(import (chicken io))
(import (chicken string))
(import srfi-69)

;(load "defs.scm") ; csc -prologue defs.scm joy.scm
(cond-expand
  (chicken-script (load "defs.scm"))
  (else))

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
    ((is-it? "+") ((joy-func +) stack expression dict))
    ((is-it? "-") ((joy-func -) stack expression dict))
    ((is-it? "*") ((joy-func *) stack expression dict))
    ((is-it? "mul") ((joy-func *) stack expression dict))

    ((is-it? "dup") (values (cons (car stack) stack) expression dict))
    ((is-it? "pop") (values (cdr stack) expression dict))
    ((is-it? "stack") (values (cons stack stack) expression dict))
    ((is-it? "swaack") (values (cons (cdr stack) (car stack)) expression dict))
    ((is-it? "swap") (values (cons (cadr stack) (cons (car stack) (cddr stack))) expression dict))

    ((is-it? "i") (joy-i stack expression dict))
    ((is-it? "dip") (joy-dip stack expression dict))
    ((is-it? "branch") (joy-branch stack expression dict))

    ((hash-table-exists? dict symbol)
      (values stack (append (hash-table-ref dict symbol) expression) dict))

    (else (error "Unknown word."))))

(define (joy-func op)
  (lambda (stack expression dict)
    (values (cons (op (cadr stack) (car stack)) (cddr stack)) expression dict)))


(define (joy-i stack expression dict)
  (values (cdr stack) (append (car stack) expression) dict))

(define (joy-dip stack expression dict)
  (values (cddr stack)
          (append (car stack) (cons (cadr stack) expression))
          dict))

(define (joy-branch stack expression dict)
  (let ((flag (caddr stack))
        (false_body (cadr stack))
        (true_body (car stack)))
    (values (cdddr stack)
            (append (if flag true_body false_body) expression)
            dict)))


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
  (load-defs (make-hash-table string=? string-hash)))

(define (load-defs dict)
  (map (lambda (def) (add-def def dict)) (defs))  ;defs is defined in defs.scm
  dict)

(define (add-def def dict)
  (let ((def_list (text->expression def)))
    (hash-table-set! dict (car def_list) (cdr def_list))))


(display (doit "true [4] [5] branch false  [4] [5] branch + sqr"))
(newline)

