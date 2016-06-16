import { CHARACTER } from './value';

export default class CharacterValue {
  constructor(value) {
    this.type = CHARACTER;
    this.value = value;
  }
  inspect() {
    return '#\\' + String.fromCharCode(this.value);
  }
}
