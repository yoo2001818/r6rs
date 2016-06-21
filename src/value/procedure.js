import { PAIR, PROCEDURE } from './value';

export default class ProcedureValue {
  constructor(name, code, args, scope) {
    this.type = PROCEDURE;
    this.name = name;
    this.args = args;
    this.code = code;
    this.scope = scope;
  }
  isNative() {
    return typeof this.code === 'function';
  }
  execute(machine, stack) { // eslint-disable-line no-unused-vars
    throw new Error('ProcedureValue subclass did not implement execute' +
      ' function.');
  }
  inspect() {
    let argsText;
    if (this.args.type === PAIR) {
      // User-defined functions have LISP list as args list
      argsText = this.args.inspect();
    } else {
      argsText = '';
    }
    return '#<procedure ' + this.name + ' ' + argsText + '>';
  }
}
