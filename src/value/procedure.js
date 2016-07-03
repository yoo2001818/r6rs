import { PAIR, PROCEDURE, SYMBOL } from './type';

export default class ProcedureValue {
  constructor(className, name, code, args, scope) {
    this.type = PROCEDURE;
    this.className = className;
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
    let argsText = '';
    if (this.args && (this.args.type === PAIR || this.args.type === SYMBOL)) {
      // User-defined functions have LISP list as args list
      argsText = this.args.inspect();
    } else if (this.args != null) {
      argsText = this.args;
    }
    return '#<' + this.className + ' ' + this.name + ' ' + argsText + '>';
  }
}
