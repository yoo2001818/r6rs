import Machine from '../src/machine';
import { PROCEDURE } from '../src/value/value';
import * as base from '../src/function/base';

import readline from 'readline';

let machine = new Machine();
for (let key in base) {
  let func = base[key];
  if (func.type !== PROCEDURE) continue;
  machine.rootParameters[func.name] = func;
}

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

console.log('tiny-lisp REPL');

let backlog = '';

function read(msg = 'scm> ') {
  rl.question(msg, (answer) => {
    try {
      console.log(machine.evaluate(backlog + answer));
      backlog = '';
    } catch (e) {
      // Reset machine stack
      machine.clearStack();
      if (e.message === 'List is not closed') {
        backlog += answer + '\n';
        read('     ');
        return;
      }
      backlog = '';
      console.log(e.stack);
    }
    read();
  });
}

read();
