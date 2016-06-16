import { BOOLEAN } from './value';

export default class BooleanValue {
  constructor(value) {
    this.type = BOOLEAN;
    this.value = value;
  }
  inspect() {
    return this.value ? '#t' : '#f';
  }
}
