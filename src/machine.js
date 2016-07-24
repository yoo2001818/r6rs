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

    // The code quota. Code execution will stop after reaching the threshold.
    // (Used to prevent infinite loop hell)
    this.quota = 0;

    // The standard I/O. Only stdout is available at this moment though.
    // Since ports are not supported yet, the stdout function should be a
    // function that accepts a string.
    if (typeof process !== 'undefined') {
      this.stdout = (str) => process.stdout.write(str);
    } else {
      // Fallback to console.log in browsers (Which is awful)
      this.stdout = (str) => console.log(str);
    }

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
  // Returns the stacktrace if the machine has abnormally exit.
  getStackTrace(fancy = false) {
    let result = [];
    let entry = this.stack;
    while (entry != null) {
      let stackEntry = entry.car;
      if (stackEntry.expression && stackEntry.expression.car) {
        let name = stackEntry.expression.car;
        if (name.line != null) {
          result.push(name.value + ' (' + name.line + ':' + name.column + ')');
        } else {
          result.push(name.value);
        }
      } else {
        // ???
        result.push('<unknown>');
      }
      entry = entry.cdr;
    }
    if (fancy) {
      return result.map(v => '    at ' + v).join('\n');
    }
    return result.join('\n');
  }
  // Stack data is passed to dryRun if mutable function is encountered;
  // Returning 'true' in that function will stop the execution, thus preventing
  // mutation.
  // Returning 'false' will continue the code normally.
  execute(dryRun = false) {
    let startStackDepth = this.stackDepth;
    let ops = 0;
    // Loop until the stack ends...
    while (this.stackDepth >= startStackDepth && this.stack != null) {
      if (this.stackDepth >= 65536) {
        throw new Error('Stack overflow');
      }
      if (this.quota > 0 && ops >= this.quota) {
        // Quota exceeded
        throw new Error('Quota exceeded');
      }
      ops ++;
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
        // If in a dry-run mode, we have to check the validity.
        if (dryRun !== false && procedure.mutable !== false &&
          dryRun(expression, procedure, stackData)) return true;
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
  evaluate(code, direct = false, libraryLevel = false, dryRun = false) {
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
      return this.execute(dryRun);
    } else {
      // Process one by one....
      let node = ast;
      let result;
      if (node.car == null) return new PairValue();
      while (node !== null && node.type === PAIR) {
        this.pushStack(node.car);
        result = this.execute(dryRun);
        if (dryRun && result === true) return true;
        node = node.cdr;
      }
      // Should we process cdr value too?
      if (node !== null) {
        this.pushStack(node);
        result = this.execute(dryRun);
        if (dryRun && result === true) return true;
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
