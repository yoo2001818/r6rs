import Machine from '../src/machine';

import ProcedureValue from '../src/procedureValue';
import * as base from '../src/function/base';

let machine = new Machine();
for (let key in base) {
  let func = base[key];
  if (typeof func !== 'function') continue;
  machine.setVariable(func.variable, new ProcedureValue([], func));
}

// We're lacking S-expression support yet. :P
let code = [
  /* ['define', ['lp', 'i'], [
    'when', ['<', 'i', ['const', 10]],
    ['display', 'i'],
    ['lp', ['+', 'i', ['const', 1]]]
  ]] */
  /* ['define', 'i', ['const', 10]],
  ['+', 'i', ['const', 5]] */
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
];

for (let i = 0; i < code.length; ++i) {
  console.log(machine.exec(code[i]));
}
