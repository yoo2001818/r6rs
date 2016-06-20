// A object representing LISP machine.
import { SYMBOL, PROCEDURE, PAIR } from './value/value';

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
  getVariable(name) {
    let node = this.stack.car.scope;
    while (node != null) {
      if (node.car[name]) return node.car[name];
      node = node.cdr;
    }
    if (this.rootParameters[name]) return this.rootParameters[name];
    throw new Error('Unbound variable: ' + name);
  }
  pushStack(list) {
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
        // Try to resolve the args value. Resolving shouldn't be done if
        // define-syntax is in use, however it won't be implemented for
        // long time.
        stackData.expTrack = expression.cdr;
        if (procedure.args && procedure.args.type === PAIR) {
          // Proceed to arguments resolving step.
          stackData.argsTrack = procedure.args;
          result = null;
        } else {
          stackData.argsTrack = null;
          stackData.buffer = {};
          // If a number is given, still, try to match the number.
          let argsNode = procedure.args;
          let expNode = expression.cdr;
          while (argsNode !== 0) {
            if (expNode == null) {
              // Missing arguments - throw an exception!
              // TODO Put actual index
              throw new Error('Argument ' + argsNode +
                ' is missing');
            }
            // It's an integer...
            argsNode --;
            expNode = expNode.cdr;
          }
          // Done!
        }
      }
      if (stackData.argsTrack != null) {
        if (result != null) {
          // If the result is present, put the data to the scope.
          let scope = stackData.scope.car;
          let argsTrack = stackData.argsTrack;
          if (stackData.argsList) {
            let pairArgs = new PairValue(stackData.result, null);
            if (stackData.argsTrack.type !== PAIR) {
              scope[argsTrack.value] = pairArgs;
            } else {
              stackData.argsTrack.cdr = pairArgs;
            }
            stackData.argsTrack = pairArgs;
          } else {
            // Normal value; just put it.
            scope[argsTrack.car.value] = stackData.result;
            // Advance to next step...
            if (argsTrack.cdr && argsTrack.cdr.type !== PAIR) {
              // Start list hell...
              stackData.argsList = true;
            }
            stackData.argsTrack = argsTrack.cdr;
          }
          stackData.expTrack = stackData.expTrack.cdr;
        }
        if (stackData.argsTrack != null) {
          if (stackData.expTrack) {
            // Try to resolve the expression value.
            this.pushStack(stackData.expTrack.car.value);
            continue;
          } else if (!stackData.argsList) {
            // Data underflow.
            throw new Error('Argument ' + stackData.argsTrack.car.inspect() +
              ' is missing');
          }
        }
      }
      let runResult = result;
      if (procedure.isNative()) {
        // Native code - Pass current stack data.
        runResult = procedure.code.call(this, stackData);
        // Increment procedure track ID.
        stackData.procTrack += 1;
        // True indicates that the executing is over.
        if (runResult === true) {
          result = stackData.result;
          // Code is complete; Return the value and release stack.
          this.stack = this.stack.cdr;
          if (this.stack) {
            // Set the result value of the stack.
            this.stack.car.result = result;
            continue;
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
            this.pushStack(code);
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
            this.stack.car.result = runResult;
          } else {
            // If no entry is available, just return the result.
            return runResult;
          }
        }
      }
    }
  }
  evaluate(code) {
    this.pushStack(code);
    return this.execute();
  }
}
