import Machine from '../src/machine';
import ProcedureValue from '../src/value/procedure';
import tokenize from '../src/tokenizer';
import parse from '../src/parser';
import * as base from '../src/function/base';

let machine = new Machine();
for (let key in base) {
  let func = base[key];
  if (typeof func !== 'function') continue;
  machine.rootParameters[func.variable] = new ProcedureValue(func.variable,
    0, func, null);
}

machine.evaluate(parse(tokenize(`
[+ 1 2 3] ; Adds 1 to 3
#|
  This is a comment.
  #|
    This is a nested comment.
  |#
|#
#; "This ignores next datum"
(define fib (lambda (n)
  (if (<= n 2)
    1
    (+ (fib (- n 1)) (fib (- n 2)))
  )
))
(fib 10)
(display "집에 가고 싶다")
(display "I want \\n \\
    to go hom\\x65")
'(#T #F #t #f)
'(#\\x65 #\\가 #\\nul)
'(yes no . cancel)
'53
`)));
