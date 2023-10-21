(import srfi-1)
(import srfi-12)
(import matchable)

; Importing srfi-67 did not actually make available symbol-compare.  Boo!

(define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))

; a BTree is a four-tuple of (name value left right) | ()

(define (btree-get key btree)
  (match btree
    (() (abort "Key not found."))
    ((k value left right)
      (if (eq? key k)
        value
        (btree-get key (if (symbol<? key k) left right))))
    (_ (abort "Not a BTree."))))


(define (btree-insert key value btree)
  (match btree
    (() (list key value '() '()))
    ((k v left right)
      (if (eq? key k)
        (list k value left right)
        (if (symbol<? key k)
          (list k v (btree-insert key value left) right)
          (list k v left (btree-insert key value right)))))
    (_ (abort "Not a BTree."))))

(set! T '())
(set! T (btree-insert 'larry 23 T))
(set! T (btree-insert 'barry 18 T))
(set! T (btree-insert 'carry 99 T))
(display T)
(newline)



(define (balance el)
  (if (null-list? el)
    el
    (balance0 el (halve (length el)))))

(define (balance0 el midpoint)
  (receive (prefix suffix) (split-at el midpoint)
    (cons
      (first suffix)
      (append 
        (balance prefix)
        (balance (cdr suffix))))))

(define (halve n) (quotient n 2))

(set! T (iota 23))

;(define (btree-sorted-list items)
;  (btree-sorted-list0 items (length items)))

;(define (btree-sorted-list items len)
;  ())
