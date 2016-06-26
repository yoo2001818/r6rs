import tokenize from '../src/tokenizer';
import parse from '../src/parser';
import expand from '../src/expander';

import Machine from '../src/machine';

let machine = new Machine();

console.log(machine.evaluate(expand(parse(tokenize(`
(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
      ((lambda (name ...) body1 body2 ...) val ...))
  )
)
(let () 3)
(let ((test 5) (test2 3)) (display "Hello") (display "World") (+ test test2))
`)))));
