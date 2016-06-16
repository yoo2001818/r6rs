import { SYMBOL } from './value';

export default class SymbolValue {
  constructor(value) {
    this.type = SYMBOL;
    this.value = value;
  }
  inspect() {
    return this.value;
  }
}
