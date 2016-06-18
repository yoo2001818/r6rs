// A object representing LISP machine.
import { SYMBOL, PROCEDURE, PAIR } from './value';

import PairValue from './value/pair';

export default class Machine {
  constructor() {
    // Stores root parameter information - base library, user defined variables
    // etc.
    this.rootParameters = {};
    // The execute stack. It uses cons (PairValue) to store information.
    // Note that this only stores 'execute' stack; scoped variables are stored
    // in different place.
    this.stack = null;
  }
  pushScope() {
    this.scopes = new PairValue({}, this.scopes);
  }
  popScope() {
    this.scopes = this.scopes.cdr;
  }
  setVariable(name, value) {
    this.scopes.car[name] = value;
  }
  getVariable(name) {
    let node = this.stack;
    while (node != null) {
      if (node.car.scope[name]) return node.car.scope[name];
      node = node.cdr;
    }
    return this.rootParameters[name];
  }
  createStackEntry(list) {
    this.stack = new PairValue({
      expression: list
    }, this.stack);
  }
  execute() {
    // Loop until the stack ends...
    while (this.stack != null) {
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
        this.stack = this.stack.cdr;
        if (this.stack) {
          // Set the result value of the stack.
          this.stack.result = runResult;
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
          if (original.type === SYMBOL) {
            procedure = this.getVariable(original.value);
          } else if (original.type === PROCEDURE) {
            procedure = original;
          } else if (original.type === PAIR) {
            // Create new stack entry and run that instead.
            this.createStackEntry(original);
            continue;
          } else {
            // Raise an exception.
            throw new Error('Procedure expected, got ' + procedure.inspect() +
              'instead');
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
            'instead');
        }
        // Set up the procTrack value - native JS code requires an integer,
        // (or something else, but 0 is the default), and Scheme procedure
        // requires an procedure (It continues by continuously getting
        // CDR value of cons)
        if (procedure.isNative()) {
          stackData.procTrack = 0;
        } else {
          stackData.procTrack = procedure.code;
        }
        // Create stack's own scope. Note that parent scope is not parent
        // stack; parent scope is marked in the procedure.
        // This shouldn't be done if the procedure is not lambda.
        stackData.scope = new PairValue({}, procedure.scope);
        let scope = stackData.scope;
        // Check and bind arguments to scope...
        let argsNode = procedure.args;
        let expNode = expression.cdr;
        while (argsNode != null) {
          // Arguments MUST be a list of symbols or an integer.
          if (argsNode.type === PAIR) {
            if (expNode == null) {
              // Missing arguments - throw an exception!
              throw new Error('Argument ' + argsNode.car.inspect() +
                ' is missing');
            }
            // Assign the variable to the scope...
            scope.car[argsNode.car.value] = expNode.car;
            // Advance to next node.
            argsNode = argsNode.cdr;
            expNode = expNode.cdr;
            // This means everything else is optional arguments.
            if (argsNode != null && argsNode.type !== PAIR) {
              scope.car[argsNode.value] = expNode;
              expNode = null;
              break;
            }
          } else {
            if (expNode == null) {
              // Missing arguments - throw an exception!
              // TODO Put actual index
              throw new Error('Argument ' + argsNode +
                ' is missing');
            }
            // It's an integer...
            argsNode --;
            expNode = expNode.cdr;
            // If expNode has something left at this point, it should throw
            // an error. However since this lacks optional arguments,
            // just continue.
            if (argsNode === 0) break;
          }
        }
        // If expNode is not null at this point, it should throw an exception.
        // Everything is ready! Let's run it!
        stackData.buffer = {};
      }
      let runResult = result;
      if (procedure.isNative()) {
        // Native code - Pass current stack data.
        runResult = procedure.code.call(this, stackData);
        // True indicates that the executing is over.
        if (runResult === true) {
          result = stackData.result;
          // Code is complete; Return the value and release stack.
          this.stack = this.stack.cdr;
          if (this.stack) {
            // Set the result value of the stack.
            this.stack.result = result;
          } else {
            // If no entry is available, just return the result.
            return result;
          }
        }
      } else {
        // Non-native code.
        let procTrack = stackData.procTrack;
        if (procTrack != null) {
          let code;
          if (procTrack.type === PAIR) {
            code = procTrack.car;
            procTrack = procTrack.cdr;
          } else {
            code = procTrack;
            procTrack = null;
          }
          stackData.procTrack = procTrack;
          if (code.type === PAIR) {
            this.createStackEntry(code);
            continue;
          } else if (code.type === SYMBOL) {
            runResult = this.getVariable(code.value);
          } else {
            runResult = code;
          }
        }
        if (procTrack == null) {
          // Code is complete; Return the value and release stack.
          this.stack = this.stack.cdr;
          if (this.stack) {
            // Set the result value of the stack.
            this.stack.result = runResult;
          } else {
            // If no entry is available, just return the result.
            return runResult;
          }
        }
      }
    }
  }
  exec(code) {
    if (Array.isArray(code)) {
      // Execute the function...
      let func = this.exec(code[0]);
      if (func && func.type === 'procedure') {
        return func.exec(this, code.slice(1));
      } else {
        throw new Error('Wrong data provided');
      }
    } else if (code.type === 'procedure' || code.type === 'const') {
      return code;
    } else {
      // Resolve the value from scope
      return this.getVariable(code);
    }
  }
}
