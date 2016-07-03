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
      (apply for-all proc (map cdr lists))
      #f
    )
  )
)

(define (exists proc . lists)
  (if (null? (car lists))
    #f
    (if (apply proc (map car lists))
      #t
      (apply exists proc (map cdr lists))
    )
  )
)

(define (filter proc input)
  ; TODO We should use set-cdr! to increase performance, if that's implemented.
  (let ((output '()))
    (for-each (lambda (x)
      (if (proc x)
        (set! output (append output (cons x '())))
      )
    ) input)
    output
  )
)

(define (partition proc input)
  ; TODO We should use set-cdr! to increase performance, if that's implemented.
  (let ((trues '()) (falses '()))
    (for-each (lambda (x)
      (if (proc x)
        (set! trues (append trues (cons x '())))
        (set! falses (append falses (cons x '())))
      )
    ) input)
    (list trues falses)
  )
)

(define (fold-left combine nil . lists)
  (let* ((output nil) (operator '())) ; letrec* is not available at this moment
    (set! operator (lambda lists
      (if (null? (car lists))
        output
        (begin
          (set! output (apply combine output (map car lists)))
          (apply operator (map cdr lists))
        )
      )
    ))
    (apply operator lists)
  )
)

(define (fold-right combine nil . lists)
  (let* ((output nil) (operator '())) ; letrec* is not available at this moment
    (set! operator (lambda lists
      (if (null? (car lists))
        output
        (begin
          (set! output (apply combine (append (map car lists) (cons output '()))))
          (apply operator (map cdr lists))
        )
      )
    ))
    (apply operator (map reverse lists))
  )
)

; `;
