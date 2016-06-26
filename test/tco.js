import Machine from '../src/machine';

let machine = new Machine();

let startTime = Date.now();

machine.evaluate(`
(define addAll (lambda (n s)
  (if (<= n 1)
    (+ s 1)
    (addAll (- n 1) (+ s n))
  )
))
(addAll 10000 0) ;This is too slow for now..
`);

console.log('Elapsed time: ' + (Date.now() - startTime));
