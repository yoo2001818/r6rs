; module.exports = `

(define real-valued? real?)
(define rational-valued? rational?)
(define integer-valued? integer?)

(define (div-and-mod x y) (list (div x y) (mod x y)))
(define (div0-and-mod0 x y) (list (div0 x y) (mod0 x y)))

(define-syntax number->string (syntax-rules ()
  ((_ a) (number->string a 10))
))

(define-syntax string->number (syntax-rules ()
  ((_ a) (string->number a 10))
))

; `;
