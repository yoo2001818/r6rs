import { STRING } from './index';

export default class StringValue {
  constructor(value) {
    this.type = STRING;
    this.value = value;
  }
  inspect() {
    let value = this.value;
    value = value.replace(/\\/g, '\\\\');
    value = value.replace(/\r/g, '\\r');
    value = value.replace(/\n/g, '\\n');
    value = value.replace(/\t/g, '\\t');
    value = value.replace(/\0/g, '\\0');
    value = value.replace(/"/g, '\\"');
    return '"' + value + '"';
  }
}
