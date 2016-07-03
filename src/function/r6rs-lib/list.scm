; module.exports = `

(define (find proc list)
  (if (null? list)
    #f
    (if (proc (car list))
      (car list)
      (find proc (cdr list))
    )
  )
)

(define (for-all proc . lists)
  (if (null? (car lists))
    #t
    (if (apply proc (map car lists))
      (for-all proc (map cdr lists))
      #f
    )
  )
)

; `;
