// A object representing LISP machine.

export default class Machine {
  constructor() {
    // Scope array - contains the scope variables information.
    this.scopes = [{}];
    // That's it for now... I think.
  }
  pushScope() {
    this.scopes.push({});
  }
  popScope() {
    this.scopes.pop();
  }
  setVariable(name, value) {
    this.scopes[this.scopes.length - 1][name] = value;
  }
  getVariable(name) {
    for (let i = this.scopes.length - 1; i >= 0; --i) {
      if (this.scopes[i][name]) return this.scopes[i][name];
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
