export default class ProcedureValue {
  constructor(args, code) {
    this.type = 'procedure';
    this.args = args;
    this.code = code;
  }
  exec(machine, args) {
    // Run the code; if the code is native JS function - just call it. If not,
    // create scope and run one by one.
    // Note that this uses JS stack - It should use its own stack to
    // manage errors etc.
    if (typeof this.code === 'function') {
      return this.code.apply(this, [machine].concat(args));
    }
    // Create scope and run it
    machine.pushScope();
    // Set scope variables
    for (let i = 0; i < this.args.length; ++i) {
      machine.setVariable(this.args[i], machine.exec(args[i]));
    }
    console.log(machine.scopes);
    // Run one by one.... This should really use Pairs though.
    let current = null;
    for (let i = 0; i < this.code.length; ++i) {
      current = machine.exec(this.code[i]);
    }
    // Done! pop the scope.
    machine.popScope();
    return current;
  }
}
