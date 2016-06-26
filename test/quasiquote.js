import Machine from '../src/machine';

let machine = new Machine();

console.log(machine.evaluate(`
(define hello 6)
(display \`(1 2 3 4 ,(+ 3 2) ,hello ,@(list 7 8 9 10) (11 ,(+ 13 1)) . 15))
(display \`(let ((a 1)) a))
(display \`(let ((a 1)) a ,(let ((a 1)) a)))
`));
