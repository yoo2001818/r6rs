// A object representing LISP machine.
import { SYMBOL, PROCEDURE, PAIR } from './value/value';

import PairValue from './value/pair';

import parse from './parser';
import tokenize from './tokenizer';
import expand from './expander';

export default class Machine {
  constructor() {
    // Stores root parameter information - base library, user defined variables
    // etc.
    this.rootParameters = {};
    // Root syntax information used by the expander.
    this.expanderRoot = {};
    // The execute stack. It uses cons (PairValue) to store information.
    // Note that this only stores 'execute' stack; scoped variables are stored
    // in different place.
    this.stack = null;
    this.stackDepth = 0;
  }
  getVariable(name) {
    // Iterate until scope appears...
    let node = this.stack && this.stack.car.scope;
    while (node != null) {
      if (node.car[name] != null) return node.car[name];
      node = node.cdr;
    }
    if (this.rootParameters[name] != null) {
      return this.rootParameters[name];
    }
    throw new Error('Unbound variable: ' + name);
  }
  jumpStack(list) {
    // Performs TCO optimization
    let stackEntry = this.stack.car;
    this.popStack();
    this.pushStack(list, stackEntry);
    stackEntry.tco = true;
  }
  clearStack() {
    this.stack = null;
    this.stackDepth = 0;
  }
  popStack() {
    this.stack = this.stack.cdr;
    this.stackDepth --;
  }
  pushStack(list, stackEntry) {
    let scope;
    if (stackEntry) {
      scope = stackEntry.scope;
    } else {
      scope = this.stack && this.stack.car.scope;
    }
    this.stack = new PairValue({
      expression: list,
      scope
    }, this.stack);
    this.stackDepth ++;
  }
  execute() {
    // Loop until the stack ends...
    while (this.stack != null) {
      if (this.stackDepth >= 65536) {
        throw new Error('Stack overflow');
      }
      let stackData = this.stack.car;
      let { expression, procedure, result } = stackData;
      if (expression.type !== PAIR) {
        // If constant values are provided...
        let runResult;
        if (expression.type === SYMBOL) {
          runResult = this.getVariable(expression.value);
        } else {
          runResult = expression;
        }
        // Code is complete; Return the value and release stack.
        this.popStack();
        if (this.stack) {
          // Set the result value of the stack.
          this.stack.car.result = runResult;
          continue;
        } else {
          // If no entry is available, just return the result.
          return runResult;
        }
      }
      if (procedure == null) {
        // Procedure is not ready yet; Is the procedure calculated?
        if (result == null) {
          // Nope! Try to resolve the procedure. If value is a symbol,
          // resolve it without creating new stack entry.
          // If value is a procedure (It's not possible...), solve it directly.
          // If value is a list (pair), create new stack entry.
          let original = expression.car;
          if (original == null) {
            throw new Error('List expected; got empty pair instead');
          }
          if (original.type === SYMBOL) {
            procedure = this.getVariable(original.value);
            stackData.procedure = procedure;
          } else if (original.type === PROCEDURE) {
            procedure = original;
            stackData.procedure = procedure;
          } else if (original.type === PAIR) {
            // Create new stack entry and run that instead.
            this.pushStack(original);
            continue;
          } else {
            // Raise an exception.
            throw new Error('Procedure expected, got ' + original.inspect() +
              ' instead');
          }
        } else {
          // If procedure is calculated, just continue.
          procedure = result;
          stackData.procedure = result;
        }
        // We have to check validity of the procedure - It should be a
        // procedure object.
        if (procedure.type !== PROCEDURE) {
          // Raise an exception; However since we lack stack rewinding and
          // stuff (such as with-exception-handler), just throw an native
          // exception.
          throw new Error('Procedure expected, got ' + procedure.inspect() +
            ' instead');
        }
      }
      // We've got the procedure - Let's pass the whole stack frame to
      // the procedure!
      let runResult = procedure.execute(this, stackData);
      // True indicates that the executing is over.
      if (runResult === true && !stackData.tco) {
        result = stackData.result;
        // Code is complete; Return the value and release stack.
        this.popStack();
        if (this.stack) {
          // Set the result value of the stack.
          this.stack.car.result = result;
          continue;
        } else {
          // If no entry is available, just return the result.
          return result;
        }
      }
    }
  }
  // This accepts AST generated by parser; it can't process raw string!
  // Direct means that the provided code should be treated as single list,
  // thus preventing separation.
  evaluate(code, direct = false) {
    let ast;
    if (typeof code === 'string') {
      ast = expand(parse(tokenize(code)), this.expanderRoot);
    } else {
      ast = code;
    }
    if (direct || ast.type !== PAIR) {
      this.pushStack(ast);
      return this.execute();
    } else {
      // Process one by one....
      let node = ast;
      let result;
      while (node !== null && node.type === PAIR) {
        this.pushStack(node.car);
        result = this.execute();
        node = node.cdr;
      }
      // Should we process cdr value too?
      if (node !== null) {
        this.pushStack(node);
        result = this.execute();
      }
      return result;
    }
  }
  // Loads platform library into the interpreter scope.
  loadLibrary(list) {
    for (let entry of list) {
      if (entry.type === PROCEDURE) {
        this.rootParameters[entry.name] = entry;
      } else if (Array.isArray(entry)) {
        this.loadLibrary(entry);
      } else if (typeof entry === 'string') {
        this.evaluate(entry);
      }
    }
  }
}
