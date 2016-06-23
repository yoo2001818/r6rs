; module.exports = `
(define list-tail (lambda (list k)
  (if (<= k 0)
    list
    (list-tail (cdr list) (- k 1))
  )
))

(define list-ref (lambda (list k)
  (car (list-tail list k))
))
; `;
