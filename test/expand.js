import tokenize from '../src/tokenizer';
import parse from '../src/parser';
import expand from '../src/expander';

console.log(expand(parse(tokenize(`
(define-syntax let
  (syntax-rules ()
    ((_ ((var val) ...) exp exp* ...)
     (let ((var val) ...) exp exp* ...))))
(let ((test 5) (test2 3)) (display "Hello") (+ test test2))
`))));
