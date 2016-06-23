import Machine from '../src/machine';
import { PROCEDURE } from '../src/value/value';
import base from '../src/function/primitive';

let machine = new Machine();
for (let func of base) {
  if (func.type !== PROCEDURE) continue;
  machine.rootParameters[func.name] = func;
}

machine.evaluate(`
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
'()
'53
`);
