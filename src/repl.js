#!/usr/bin/env node

import Machine from './machine';
import fs from 'fs';
import readline from 'readline';

let machine = new Machine();

if (process.argv[2]) {
  let data = fs.readFileSync(process.argv[2], 'utf-8');
  machine.evaluate(data);
} else {
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });

  console.log('r6rs REPL');

  let backlog = '';

  const read = (msg = 'scm> ') => {
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
  };

  read();
}
