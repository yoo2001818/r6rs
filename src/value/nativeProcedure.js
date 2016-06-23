import ProcedureValue from './procedure';
import PairValue from './pair';

export default class NativeProcedureValue extends ProcedureValue {
  constructor(name, code) {
    super(name, code);
  }
  execute(machine, frame) {
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
        machine.pushStack(frame.expTrack.car);
        return;
      } else {
        // Start processing.
        frame.procTrack = 0;
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
