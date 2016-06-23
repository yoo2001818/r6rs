import tokenize from '../src/tokenizer';
import parse from '../src/parser';
import expand from '../src/expander';

import Machine from '../src/machine';
import { PROCEDURE } from '../src/value/value';
import * as base from '../src/function/base';

let machine = new Machine();
for (let key in base) {
  let func = base[key];
  if (func.type !== PROCEDURE) continue;
  machine.rootParameters[func.name] = func;
}

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
