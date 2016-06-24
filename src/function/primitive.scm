; module.exports = `
(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
       val ...))
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...) body1 body2 ...))) tag)
       val ...))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 expr1) (name2 expr2) ...) body1 body2 ...)
     (let ((name1 expr1))
       (let* ((name2 expr2) ...) body1 body2 ...)))))

; letrec, letrec*, let-values, let*-values

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))

(define-syntax define
  (syntax-rules ()
    ((define var) (define var '()))
    ((define (var formals ...) body1 body2 ...)
      (define var (lambda (formals ...) body1 body2 ...)))
    ((define (var . formal) body1 body2 ...)
      (define var (lambda formal body1 body2 ...)))
  )
)

(define (eq? a b) (eqv? a b))

; `;
