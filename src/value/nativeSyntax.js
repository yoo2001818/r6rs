import ProcedureValue from './procedure';
import PairValue from './pair';
import { PAIR } from './type';

export default class NativeSyntaxValue extends ProcedureValue {
  constructor(name, code, args, argsExtra) {
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
    super('syntax', name, code, argsPair);
  }
  execute(machine, frame) {
    // Completely skip the arguments checking code.
    if (frame.procTrack == null) {
      frame.procTrack = 0;
      frame.expTrack = frame.expression.cdr;
    }
    // Start executing the code! :P
    let result = frame.procedure.code(machine, frame);
    frame.procTrack += 1;
    return result;
  }
}
