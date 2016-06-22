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

function read() {
  rl.question('scm> ', (answer) => {
    try {
      console.log(machine.evaluate(answer));
    } catch (e) {
      console.log(e.stack);
      // Reset machine stack
      machine.clearStack();
    }
    read();
  });
}

read();
