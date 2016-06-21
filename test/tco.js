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
(define addAll (lambda (n s)
  (if (<= n 1)
    (+ s 1)
    (addAll (- n 1) (+ s n))
  )
))
(addAll 1000000 0) ;This is too slow for now..
`)));
