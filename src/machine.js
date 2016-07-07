// A object representing LISP machine.
import { SYMBOL, PROCEDURE, PAIR } from './value';

import PairValue from './value/pair';

import parse from './parser';
import tokenize from './tokenizer';
import expand from './expander';

import base from './function';

export default class Machine {
  constructor(loadBase = true, libraryCache) {
    // Stores root parameter information - user defined variables, functions,
    // etc.
    this.rootParameters = {};
    // Root syntax information used by the expander.
    this.expanderRoot = {};

    if (libraryCache != null) {
      if (libraryCache.parameters == null) {
        libraryCache.parameters = {};
      }
      if (libraryCache.expands == null) {
        libraryCache.expands = {};
      }
      this.libraryParameters = libraryCache.parameters;
      this.expanderLibrary = libraryCache.expands;
    } else {
      // Stores library parameter information - base library, SRFI, etc.
      this.libraryParameters = {};
      // Library root syntax information used by the expander.
      this.expanderLibrary = {};
    }

    // The execute stack. It uses cons (PairValue) to store information.
    // Note that this only stores 'execute' stack; scoped variables are stored
    // in different place.
    this.stack = null;
    this.stackDepth = 0;
    this.libraryLevel = false;
    if (loadBase) this.loadLibrary(base);
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
    if (this.libraryParameters[name] != null) {
      return this.libraryParameters[name];
    }
    throw new Error('Unbound variable: ' + name);
  }
  jumpStack(list, noResolve) {
    // Performs TCO optimization
    let stackEntry = this.stack.car;
    this.popStack();
    this.pushStack(list, stackEntry, noResolve);
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
  pushStack(list, stackEntry, noResolve) {
    let scope;
    if (stackEntry) {
      scope = stackEntry.scope;
    } else {
      scope = this.stack && this.stack.car.scope;
    }
    this.stack = new PairValue({
      expression: list,
      scope, noResolve
    }, this.stack);
    this.stackDepth ++;
  }
  execute() {
    let startStackDepth = this.stackDepth;
    // Loop until the stack ends...
    while (this.stackDepth >= startStackDepth && this.stack != null) {
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
        if (this.stackDepth >= startStackDepth && this.stack) {
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
        if (this.stackDepth >= startStackDepth && this.stack) {
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
  // Library level means the evaluation is running in the library level, thus
  // it should define variables in library scope.
  evaluate(code, direct = false, libraryLevel = false) {
    let ast;
    if (typeof code === 'string') {
      if (libraryLevel) {
        ast = expand(parse(tokenize(code)), this.expanderLibrary);
      } else {
        ast = expand(parse(tokenize(code)), this.expanderRoot,
          new PairValue(this.expanderLibrary));
      }
    } else {
      ast = code;
    }
    if (ast == null) return new PairValue();
    this.libraryLevel = libraryLevel;
    if (direct || ast.type !== PAIR) {
      this.pushStack(ast);
      return this.execute();
    } else {
      // Process one by one....
      let node = ast;
      let result;
      if (node.car == null) return new PairValue();
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
        this.libraryParameters[entry.name] = entry;
      } else if (Array.isArray(entry)) {
        this.loadLibrary(entry);
      } else if (typeof entry === 'string') {
        this.evaluate(entry, false, true);
      }
    }
  }
}
