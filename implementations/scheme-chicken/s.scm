(define (string-replace str from to)
  (string-intersperse (string-split str from) to)
  )

(define (tokenize str)
  (string-split
    (string-replace 
      (string-replace str "]" " ] ")
      "[" " [ "
      )
    )
  )


