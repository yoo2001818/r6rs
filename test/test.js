import Machine from '../src/machine';

import ProcedureValue from '../src/value/procedure';
import PairValue from '../src/value/pair';
import StringValue from '../src/value/string';
import SymbolValue from '../src/value/symbol';
import * as base from '../src/function/base';

let machine = new Machine();
for (let key in base) {
  let func = base[key];
  if (typeof func !== 'function') continue;
  machine.rootParameters[func.variable] = new ProcedureValue(func.variable,
    0, func, null);
}
console.log(machine.evaluate(PairValue.fromArray([
  new SymbolValue('define'),
  new SymbolValue('a'),
  PairValue.fromArray([
    new SymbolValue('lambda'),
    PairValue.fromArray([
      new SymbolValue('x')
    ]),
    new SymbolValue('x')
  ])
]), true));

console.log(machine.evaluate(PairValue.fromArray([
  new SymbolValue('a'),
  new StringValue('Hello!')
]), true));

// We're lacking S-expression support yet. :P
/*let code = [
  /* ['define', ['lp', 'i'], [
    'when', ['<', 'i', ['const', 10]],
    ['display', 'i'],
    ['lp', ['+', 'i', ['const', 1]]]
  ]] */
  /* ['define', 'i', ['const', 10]],
  ['+', 'i', ['const', 5]]
  ['define', 'fib', ['lambda', ['n'],
    ['if', ['<=', 'n', ['const', 2]],
      ['const', 1],
      ['+',
        ['fib', ['-', 'n', ['const', 1]]],
        ['fib', ['-', 'n', ['const', 2]]]
      ]
    ]
  ]],
  ['fib', ['const', 10]]
];*/
/*
for (let i = 0; i < code.length; ++i) {
  console.log(machine.exec(code[i]));
}
*/
