import { SYMBOL, PAIR } from './value/value';

import PairValue from './value/pair';

import SyntaxRules from './transformer/syntaxRules';

// Statically expands the AST using macros... This is equvalent to
// macro-expansion time (specified in R6RS)

const transformers = {
  'syntax-rules': code => new SyntaxRules(code)
};

const basicKeywords = {
  'lambda': (frame, code, stack) => {
    // Create new scope and stack
    return new PairValue({
      code,
      scope: new PairValue({}, frame.scope)
    }, stack);
  },
  'define-syntax': (frame, code, stack, rootScope) => {
    // Create the transformer object
    let name = code.cdr.car;
    if (!name || name.type !== SYMBOL) {
      throw new Error('define-syntax expects a symbol');
    }
    if (rootScope[name.value]) {
      throw new Error('Syntax ' + name.value + ' conflicts');
    }
    let transCode = code.cdr.cdr.car;
    let transName = transCode.car;
    if (!transName || transName.type !== SYMBOL) {
      throw new Error('Transformer name must be a symbol');
    }
    let transFunc = transformers[transName.value];
    if (!transFunc) {
      throw new Error('Unsupported transformer type');
    }
    let transObj = transFunc(transCode);
    console.log(transObj);
    // Apply the transform object to root scope
    rootScope[name.value] = transObj;
    // Don't process child nodes
    frame.result = code;
    return stack;
  }
};

function findScope(scope, name) {
  let scopeNode = scope;
  while (scopeNode != null) {
    if (scopeNode.car[name]) {
      // Found it!
      return scopeNode.car[name];
    }
    scopeNode = scopeNode.cdr;
  }
}

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
        if (code.car && code.car.type === SYMBOL) {
          let keywordHandler = basicKeywords[code.car.value];
          if (keywordHandler) {
            stack = keywordHandler(frame, code, stack, rootScope);
            continue;
          }
        }
        // Create child stack frame.
        stack = new PairValue({
          code,
          scope: frame.scope
        }, stack);
        continue;
      } else {
        code = frame.result;
        if (code.car && code.car.type === SYMBOL) {
          // Traverse scope information, and find the symbol
          let transformer = findScope(frame.scope, code.car.value);
          if (transformer) {
            code = transformer.exec(frame.result);
          }
        }
        frame.result = null;
      }
    } else if (code && code.type === SYMBOL) {
      // Is symbol itself subject to transformation? I suppose not.
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
