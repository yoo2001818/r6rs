import ProcedureValue from './procedure';
import PairValue from './pair';

import { PAIR, SYMBOL } from './type';

export default class LambdaValue extends ProcedureValue {
  constructor(name, args, code, scope) {
    super(name, args, code, scope);
  }
  execute(machine, frame) {
    if (frame.procTrack == null) {
      // Create scope and put variables into it, if it isn't specified.
      if (frame.scopeTrack == null) {
        let { procedure, expression } = frame;
        // Create stack's own scope. Note that parent scope is not parent
        // stack; parent scope is marked in the procedure.
        frame.scopeTrack = new PairValue({}, procedure.scope);
        frame.expTrack = expression.cdr;
        frame.argsTrack = procedure.args;
        frame.result = null;
      }
      if (frame.result != null) {
        // If the result is present, put the data to the scope.
        let scope = frame.scopeTrack.car;
        if (frame.argsList) {
          let pairArgs = new PairValue(frame.result, null);
          if (frame.argsTrack.type !== PAIR) {
            scope[frame.argsTrack.value] = pairArgs;
          } else {
            frame.argsTrack.cdr = pairArgs;
          }
          frame.argsTrack = pairArgs;
        } else {
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
      if (frame.argsTrack != null && frame.argsTrack.car != null) {
        if (frame.expTrack) {
          // Try to resolve the expression value.
          machine.pushStack(frame.expTrack.car);
          return;
        } else if (!frame.argsList) {
          // Data underflow.
          throw new Error('Argument ' + frame.argsTrack.car.inspect() +
            ' is missing');
        }
      } else {
        // Finalize scope and start processing.
        frame.scope = frame.scopeTrack;
        frame.procTrack = frame.procedure.code;
      }
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
