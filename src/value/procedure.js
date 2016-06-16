import { PAIR, PROCEDURE } from './value';

export default class ProcedureValue {
  constructor(name, args, code) {
    this.type = PROCEDURE;
    this.name = name;
    this.args = args;
    this.code = code;
  }
  inspect() {
    let argsText;
    if (this.args.type === PAIR) {
      // User-defined functions have LISP list as args list
      argsText = this.args.inspect();
    } else {
      // Native functions only have number for args.
      // TODO optional args support for native functions
      argsText = '';
      for (let i = 0; i < this.args; ++i) {
        if (i === this.args - 1) {
          argsText += '_';
        } else {
          argsText += '_ ';
        }
      }
    }
    return '#<procedure ' + name + ' ' + argsText + '>';
  }
}
