import ProcedureValue from './procedure';
import PairValue from './pair';
import { PAIR } from './type';

export default class NativeProcedureValue extends ProcedureValue {
  constructor(name, code, args, argsExtra, mutable = false) {
    let argsPair = null;
    if (args) argsPair = PairValue.fromArray(args);
    if (argsExtra != null) {
      if (argsPair == null) {
        argsPair = argsExtra;
      } else {
        let node = argsPair;
        while (node != null && node.cdr != null && node.cdr.type === PAIR) {
          node = node.cdr;
        }
        node.cdr = argsExtra;
      }
    }
    super('procedure<native>', name, code, argsPair, undefined, mutable);
  }
  execute(machine, frame, dryRun) {
    if (frame.procTrack == null) {
      // Put everything to the list... And no, we don't need scope
      if (frame.list == null) {
        let { expression } = frame;
        frame.expTrack = expression.cdr;
        frame.listTrack = false;
        frame.list = false;
        frame.result = null;
      }
      if (frame.result != null) {
        let pair = new PairValue(frame.result, null);
        if (!frame.list) {
          frame.list = pair;
          frame.listTrack = pair;
        } else {
          frame.listTrack.cdr = pair;
          frame.listTrack = pair;
        }
        frame.expTrack = frame.expTrack.cdr;
      }
      if (frame.expTrack != null) {
        // Try to resolve the expression value.
        if (frame.noResolve) {
          frame.result = frame.expTrack.car || new PairValue();
          return;
        }
        machine.pushStack(frame.expTrack.car);
        return;
      } else {
        // Start processing.
        frame.procTrack = 0;
        // If in a dry-run mode, we have to check the validity.
        if (dryRun !== false && frame.procedure.mutable !== false &&
          dryRun(frame.expression, frame.procedure, frame)) return 'dryRun';
        if (frame.stop) return true;
      }
    }
    // Start executing the code! :P
    let result = frame.procedure.code(frame.list, machine, frame);
    if (result !== undefined) {
      frame.result = result;
      return true;
    }
    frame.procTrack += 1;
    return result;
  }
}
