import { SYMBOL, PROCEDURE, PAIR } from './value/value';

import PairValue from './value/pair';

// Statically expands the AST using macros... This is equvalent to
// macro-expansion time (specified in R6RS)

// Root scope stores the 'define-syntax' status.
export default function expand(code, rootScope = {}) {
  // Well, we should visit whole node and perform transformation if required.
  // Traverse ALL the node!!
  let stack = new PairValue({
    code: code,
    scope: new PairValue(rootScope)
  });
  while (stack != null) {
    let frame = stack.car;
    let code = frame.code;
    if (frame.code.type === PAIR) code = frame.code.car;
    // Process the node...
    if (code && code.type === PAIR) {
      // Traverse child node! ... but before doing that, we should determine
      // if the pair is subject to expand.
      // Or, we should check if it creates new scope (lambda), or it defines
      // new syntax. (define-syntax, let-syntax, etc)
      if (frame.result == null) {
        // Create child stack frame.
        stack = new PairValue({
          code,
          scope: frame.scope
        }, stack);
        continue;
      } else {
        code = frame.result;
        frame.result = null;
      }
    }
    console.log(code);
    // Copy current node to the output.
    if (frame.code.type === PAIR) {
      let newPair = new PairValue(code, null);
      if (frame.outputTail) {
        frame.outputTail.cdr = newPair;
        frame.outputTail = newPair;
      } else {
        frame.outputHead = newPair;
        frame.outputTail = newPair;
      }
    } else {
      // Handle CDR value differently...
      if (frame.outputTail) {
        frame.outputTail.cdr = code;
      } else {
        let newPair = new PairValue(null, code);
        frame.outputHead = newPair;
        frame.outputTail = newPair;
      }
    }
    // Continue to next stack entry.
    if (frame.code.type === PAIR) {
      frame.code = frame.code.cdr;
    } else {
      frame.code = null;
    }
    if (frame.code == null) {
      // Done! Pop current stack..
      stack = stack.cdr;
      if (stack != null) {
        stack.car.result = frame.outputHead;
      } else {
        // Traversal finished. return current output...
        return frame.outputHead;
      }
    }
  }
}
