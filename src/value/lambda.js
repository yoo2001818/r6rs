import ProcedureValue from './procedure';
import PairValue from './pair';

import { PAIR, SYMBOL } from './type';

export default class LambdaValue extends ProcedureValue {
  constructor(name, code, args, scope) {
    // Lambda procedure can't be mutable by itself
    super('procedure', name, code, args, scope, false);
  }
  execute(machine, frame, dryRun) {
    if (frame.procTrack == null) {
      // Create scope and put variables into it, if it isn't specified.
      if (frame.scopeTrack == null) {
        let { procedure, expression } = frame;
        // Create stack's own scope. Note that parent scope is not parent
        // stack; parent scope is marked in the procedure.
        frame.scopeTrack = new PairValue({}, procedure.scope);
        frame.expTrack = expression.cdr;
        frame.argsTrack = procedure.args;
        if (frame.argsTrack.type !== PAIR) {
          frame.argsList = true;
        }
        frame.result = null;
      }
      if (frame.result != null) {
        // If the result is present, put the data to the scope.
        let scope = frame.scopeTrack.car;
        if (frame.argsList) {
          let pairArgs = new PairValue(frame.result, null);
          if (frame.argsTrack.type !== PAIR) {
            if (frame.argsTrack.type !== SYMBOL) {
              throw new Error('Symbol expected, ' + frame.argsTrack.inspect() +
                ' received');
            }
            scope[frame.argsTrack.value] = pairArgs;
          } else {
            frame.argsTrack.cdr = pairArgs;
          }
          frame.argsTrack = pairArgs;
        } else {
          if (frame.argsTrack.car.type !== SYMBOL) {
            throw new Error('Symbol expected, ' + frame.argsTrack.inspect() +
              ' received');
          }
          // Normal value; just put it.
          scope[frame.argsTrack.car.value] = frame.result;
          // Advance to next step...
          if (frame.argsTrack.cdr && frame.argsTrack.cdr.type !== PAIR) {
            // Start list hell...
            frame.argsList = true;
          }
          frame.argsTrack = frame.argsTrack.cdr;
        }
        frame.expTrack = frame.expTrack.cdr;
      }
      if (frame.argsTrack != null &&
        (frame.argsTrack.type != PAIR || frame.argsTrack.car != null)
      ) {
        if (frame.expTrack) {
          // Try to resolve the expression value.
          if (frame.noResolve) {
            frame.result = frame.expTrack.car || new PairValue();
            return;
          }
          machine.pushStack(frame.expTrack.car);
          return;
        } else if (!frame.argsList) {
          // Data underflow.
          throw new Error('Argument ' + frame.argsTrack.car.inspect() +
            ' is missing');
        } else if (frame.argsTrack.type !== PAIR) {
          // if argsTrack is not pair, try to add empty list to it
          if (frame.argsTrack.type !== SYMBOL) {
            throw new Error('Symbol expected, ' + frame.argsTrack.inspect() +
              ' received');
          }
          frame.scopeTrack.car[frame.argsTrack.value] = new PairValue();
        }
      }
      // Finalize scope and start processing.
      frame.scope = frame.scopeTrack;
      frame.procTrack = frame.procedure.code;
      // If in a dry-run mode, we have to check the validity.
      if (dryRun !== false && frame.procedure.mutable !== false &&
        dryRun(frame.expression, frame.procedure, frame)) return 'dryRun';
    }
    // Start executing the code! :P
    let code;
    if (frame.procTrack.type === PAIR) {
      code = frame.procTrack.car;
      frame.procTrack = frame.procTrack.cdr;
    } else {
      code = frame.procTrack;
      frame.procTrack = null;
    }
    if (code.type === PAIR) {
      if (frame.procTrack == null) {
        // TCO!
        machine.jumpStack(code);
        return true;
      } else {
        machine.pushStack(code);
      }
    } else if (code.type === SYMBOL) {
      if (frame.procTrack == null) {
        frame.result = machine.getVariable(code.value);
        return true;
      }
    } else {
      if (frame.procTrack == null) {
        frame.result = code;
        return true;
      }
    }
  }
}
