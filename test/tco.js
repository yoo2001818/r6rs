import Machine from '../src/machine';
import { PROCEDURE } from '../src/value/value';
import tokenize from '../src/tokenizer';
import parse from '../src/parser';
import * as base from '../src/function/base';

let machine = new Machine();
for (let key in base) {
  let func = base[key];
  if (func.type !== PROCEDURE) continue;
  machine.rootParameters[func.name] = func;
}

let startTime = Date.now();

machine.evaluate(parse(tokenize(`
(define addAll (lambda (n s)
  (if (<= n 1)
    (+ s 1)
    (addAll (- n 1) (+ s n))
  )
))
(addAll 1000000 0) ;This is too slow for now..
`)));

console.log('Elapsed time: ' + (Date.now() - startTime));
