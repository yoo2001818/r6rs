import { STRING } from './value';

export default class StringValue {
  constructor(value) {
    this.type = STRING;
    this.value = value;
  }
  inspect() {
    // TODO Escape \n, \r, etc
    return '"' + this.value + '"';
  }
}
