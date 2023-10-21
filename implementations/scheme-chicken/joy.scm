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
(import srfi-1)
(import srfi-12)
(import srfi-69)
(import matchable)

;(load "defs.scm") ; csc -prologue defs.scm joy.scm
(cond-expand
  (chicken-script (load "defs.scm"))
  (else))


;██╗███╗   ██╗████████╗███████╗██████╗ ██████╗ ██████╗ ███████╗████████╗███████╗██████╗
;██║████╗  ██║╚══██╔══╝██╔════╝██╔══██╗██╔══██╗██╔══██╗██╔════╝╚══██╔══╝██╔════╝██╔══██╗
;██║██╔██╗ ██║   ██║   █████╗  ██████╔╝██████╔╝██████╔╝█████╗     ██║   █████╗  ██████╔╝
;██║██║╚██╗██║   ██║   ██╔══╝  ██╔══██╗██╔═══╝ ██╔══██╗██╔══╝     ██║   ██╔══╝  ██╔══██╗
;██║██║ ╚████║   ██║   ███████╗██║  ██║██║     ██║  ██║███████╗   ██║   ███████╗██║  ██║
;╚═╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝╚══════╝   ╚═╝   ╚══════╝╚═╝  ╚═╝
;Interpreter

(define (joy stack expression dict)
  ;(joy-trace stack expression)
  (if (null? expression)
    (values stack dict)
    (if (symbol? (car expression))
      (receive (s e d)
        (joy-eval (car expression) stack (cdr expression) dict)
        (joy s e d))
      (joy (cons (car expression) stack) (cdr expression) dict))))

(define (joy-eval symbol stack expression dict)
  (case symbol
    ((+ add) (values (joy-math-func + stack) expression dict))
    ((- sub) (values (joy-math-func - stack) expression dict))
    ((* mul) (values (joy-math-func * stack) expression dict))
    ((/ div) (values (joy-math-func quotient stack) expression dict))  ; but for negative divisor, no!?
    ((% mod) (values (joy-math-func modulo stack) expression dict))

    ((< lt) (joy-func < stack expression dict))
    ((> gt) (joy-func > stack expression dict))
    ((<= le) (joy-func <= stack expression dict))
    ((>= ge) (joy-func >= stack expression dict))
    ((= eq) (joy-func = stack expression dict))
    ((<> != neq) (joy-func not-equal stack expression dict))

    ((bool) (joy-bool stack expression dict))

    ((dup) (values (cons (car stack) stack) expression dict))
    ((pop) (values (cdr stack) expression dict))
    ((stack) (values (cons stack stack) expression dict))
    ((swaack) (values (cons (cdr stack) (car stack)) expression dict))
    ((swap) (values (cons (cadr stack) (cons (car stack) (cddr stack))) expression dict))

    ((concat) (joy-func append stack expression dict))
    ((cons) (joy-func cons stack expression dict))
    ((first) (values (joy-first stack) expression dict))
    ((rest)  (values (joy-rest  stack) expression dict))

    ((i) (joy-i stack expression dict))
    ((dip) (joy-dip stack expression dict))
    ((branch) (joy-branch stack expression dict))
    ((loop) (joy-loop stack expression dict))

    (else (if (hash-table-exists? dict symbol)
      (values stack (append (hash-table-ref dict symbol) expression) dict)
      (error (conc "Unknown word: " symbol))))))


;██╗   ██╗████████╗██╗██╗     ███████╗
;██║   ██║╚══██╔══╝██║██║     ██╔════╝
;██║   ██║   ██║   ██║██║     ███████╗
;██║   ██║   ██║   ██║██║     ╚════██║
;╚██████╔╝   ██║   ██║███████╗███████║
; ╚═════╝    ╚═╝   ╚═╝╚══════╝╚══════╝
; Utils

(define (not-equal a b) (not (= a b)))

(define (joy-func op stack expression dict)
  (values (cons (op (cadr stack) (car stack)) (cddr stack)) expression dict))

(define (joy-math-func op stack0)
  (receive (a stack1) (pop-int stack0)
  (receive (b stack) (pop-int stack1)
  (cons (op b a) stack))))

(define (pop-any stack)
  (if (null-list? stack)
    (abort "Not enough values on Stack")
    (car+cdr stack)))

(define (pop-kind stack predicate message)
  (receive (term rest) (pop-any stack)
    (if (predicate term) (values term rest) (abort message))))

(define (pop-list stack) (pop-kind stack list? "Not a list."))
(define (pop-int stack) (pop-kind stack number? "Not an integer."))
(define (pop-bool stack) (pop-kind stack boolean? "Not a Boolean value."))


; ██████╗ ██████╗ ██████╗ ███████╗    ██╗    ██╗ ██████╗ ██████╗ ██████╗ ███████╗
;██╔════╝██╔═══██╗██╔══██╗██╔════╝    ██║    ██║██╔═══██╗██╔══██╗██╔══██╗██╔════╝
;██║     ██║   ██║██████╔╝█████╗      ██║ █╗ ██║██║   ██║██████╔╝██║  ██║███████╗
;██║     ██║   ██║██╔══██╗██╔══╝      ██║███╗██║██║   ██║██╔══██╗██║  ██║╚════██║
;╚██████╗╚██████╔╝██║  ██║███████╗    ╚███╔███╔╝╚██████╔╝██║  ██║██████╔╝███████║
; ╚═════╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝     ╚══╝╚══╝  ╚═════╝ ╚═╝  ╚═╝╚═════╝ ╚══════╝
;Core Words

(define (joy-bool stack expression dict)
  (values (cons (joy-bool-term (car stack)) (cdr stack)) expression dict))

(define (joy-bool-term term)
  (cond ((boolean? term) term)
        ((number? term) (not-equal 0 term))
        ((list? term) (not (null? term)))
        (else #t)))

(define (joy-rest stack0)
  (receive (el stack) (pop-list stack0)
    (if (null-list? el)
      (abort "Cannot take rest of empty list.")
      (cons (cdr el) stack))))

(define (joy-first stack0)
  (receive (el stack) (pop-list stack0)
    (if (null-list? el)
      (abort "Cannot take first of empty list.")
      (cons (car el) stack))))


; ██████╗ ██████╗ ███╗   ███╗██████╗ ██╗███╗   ██╗ █████╗ ████████╗ ██████╗ ██████╗ ███████╗
;██╔════╝██╔═══██╗████╗ ████║██╔══██╗██║████╗  ██║██╔══██╗╚══██╔══╝██╔═══██╗██╔══██╗██╔════╝
;██║     ██║   ██║██╔████╔██║██████╔╝██║██╔██╗ ██║███████║   ██║   ██║   ██║██████╔╝███████╗
;██║     ██║   ██║██║╚██╔╝██║██╔══██╗██║██║╚██╗██║██╔══██║   ██║   ██║   ██║██╔══██╗╚════██║
;╚██████╗╚██████╔╝██║ ╚═╝ ██║██████╔╝██║██║ ╚████║██║  ██║   ██║   ╚██████╔╝██║  ██║███████║
; ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═════╝ ╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚══════╝
;Combinators

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

(define (joy-loop stack expression dict)
  (let ((flag (cadr stack))
        (body (car stack)))
    (values (cddr stack)
            (if flag (append body (cons body (cons "loop" expression))) expression)
            dict)))


;██████╗  █████╗ ██████╗ ███████╗███████╗██████╗
;██╔══██╗██╔══██╗██╔══██╗██╔════╝██╔════╝██╔══██╗
;██████╔╝███████║██████╔╝███████╗█████╗  ██████╔╝
;██╔═══╝ ██╔══██║██╔══██╗╚════██║██╔══╝  ██╔══██╗
;██║     ██║  ██║██║  ██║███████║███████╗██║  ██║
;╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝
;Parser

(define (string-replace str from to)
  (string-intersperse (string-split str from #t) to))

(define (tokenize str)
  (string-split
    (string-replace (string-replace str "]" " ] ") "[" " [ ")))

(define (tokenator token)
  (cond ((string->number token) (string->number token))
        ((string=? token "true") #t)
        ((string=? token "false") #f)
        (else (string->symbol token))))

(define (expect-right-bracket tokens acc) 
  (if (null? tokens)
    (error "Missing closing bracket.")
    (expect-right-bracket-lookahead (car tokens) (cdr tokens) acc)))

(define (expect-right-bracket-lookahead token tokens acc)
  (match token
    ("]" (values acc tokens))
    ("[" (receive (sub_list rest) (expect-right-bracket tokens '())
           (receive (el rrest) (expect-right-bracket rest acc)
             (values (cons sub_list el) rrest))))
    (_ (receive (el rest) (expect-right-bracket tokens acc)
       (values (cons (tokenator token) el) rest)))))

(define (one-token-lookahead token tokens)
  (match token
    ("]" (error "Extra closing bracket."))
    ("[" (expect-right-bracket tokens '()))
    (_ (values (tokenator token) tokens))))

(define (parse0 tokens acc)
  (if (null? tokens)
    acc
    (receive (term rest_of_tokens)
      (one-token-lookahead (car tokens) (cdr tokens))
      (cons term (parse0 rest_of_tokens acc)))))

(define (parse tokens) (parse0 tokens '()))

(define (text->expression text) (parse (tokenize text)))


;██████╗ ██████╗ ██╗███╗   ██╗████████╗███████╗██████╗
;██╔══██╗██╔══██╗██║████╗  ██║╚══██╔══╝██╔════╝██╔══██╗
;██████╔╝██████╔╝██║██╔██╗ ██║   ██║   █████╗  ██████╔╝
;██╔═══╝ ██╔══██╗██║██║╚██╗██║   ██║   ██╔══╝  ██╔══██╗
;██║     ██║  ██║██║██║ ╚████║   ██║   ███████╗██║  ██║
;╚═╝     ╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝╚═╝  ╚═╝
;Printer

(define (joy-term->string term)
  (cond ((boolean? term) (if term "true" "false"))
        ((number? term) (->string term))
        ((list? term) (conc "[" (joy-expression->string term) "]"))
        (else (symbol->string term))))

(define (joy-expression->string expr)
  (string-intersperse (map joy-term->string expr) " "))


;██████╗ ███████╗███████╗██╗███╗   ██╗██╗████████╗██╗ ██████╗ ███╗   ██╗███████╗
;██╔══██╗██╔════╝██╔════╝██║████╗  ██║██║╚══██╔══╝██║██╔═══██╗████╗  ██║██╔════╝
;██║  ██║█████╗  █████╗  ██║██╔██╗ ██║██║   ██║   ██║██║   ██║██╔██╗ ██║███████╗
;██║  ██║██╔══╝  ██╔══╝  ██║██║╚██╗██║██║   ██║   ██║██║   ██║██║╚██╗██║╚════██║
;██████╔╝███████╗██║     ██║██║ ╚████║██║   ██║   ██║╚██████╔╝██║ ╚████║███████║
;╚═════╝ ╚══════╝╚═╝     ╚═╝╚═╝  ╚═══╝╚═╝   ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚══════╝
;Definitions

(define (initialize)
  (load-defs! (make-hash-table equal? symbol-hash)))

(define (load-defs! dict)
  (for-each (lambda (def) (add-def! def dict)) (defs))
  ; defs is defined in defs.scm
  dict)

(define (add-def! def dict)
  (let ((def_list (text->expression def)))
    (hash-table-set! dict (car def_list) (cdr def_list))))


;██████╗ ███████╗██████╗ ██╗
;██╔══██╗██╔════╝██╔══██╗██║
;██████╔╝█████╗  ██████╔╝██║
;██╔══██╗██╔══╝  ██╔═══╝ ██║
;██║  ██║███████╗██║     ███████╗
;╚═╝  ╚═╝╚══════╝╚═╝     ╚══════╝
;REPL

(define (prompt) (display "joy? ") (read-line))

(define (main-loop stack0 dict0)
  (let ((text (prompt)))
    (if (eof-object? text)
      (print)
      (receive (stack dict)
        (handle-exceptions exn
          (begin (display exn) (newline) (values stack0 dict0))
          (joy stack0 (text->expression text) dict0))
        (print (joy-expression->string (reverse stack)))
        (main-loop stack dict)))))

(define (joy-trace stack expression)
  (print (conc (joy-expression->string (reverse stack)) " . " (joy-expression->string expression))))

(main-loop '() (initialize))


;(display (text->expression "5 [] cons [4] concat first"))
;(display (doit "5 down_to_zero"))
;(display (doit "1 2 true [4 5 false] loop <"))
;(newline)
