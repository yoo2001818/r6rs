import ProcedureValue from './procedure';

export default class NativeSyntaxValue extends ProcedureValue {
  constructor(name, code) {
    super(name, code);
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
