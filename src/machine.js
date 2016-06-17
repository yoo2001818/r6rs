// A object representing LISP machine.

import PairValue from './value/pair';

export default class Machine {
  constructor() {
    // Scope array - contains the scope variables information.
    this.scopes = new PairValue({});
    // That's it for now... I think.
  }
  pushScope() {
    this.scopes = new PairValue({}, this.scopes);
  }
  popScope() {
    this.scopes = this.scopes.cdr;
  }
  setVariable(name, value) {
    this.scopes.car[name] = value;
  }
  getVariable(name) {
    let node = this.scopes;
    while (node != null) {
      console.log(node);
      if (node.car[name]) return node.car[name];
      node = node.cdr;
    }
  }
  exec(code) {
    if (Array.isArray(code)) {
      // Execute the function...
      let func = this.exec(code[0]);
      if (func && func.type === 'procedure') {
        return func.exec(this, code.slice(1));
      } else {
        throw new Error('Wrong data provided');
      }
    } else if (code.type === 'procedure' || code.type === 'const') {
      return code;
    } else {
      // Resolve the value from scope
      return this.getVariable(code);
    }
  }
}
