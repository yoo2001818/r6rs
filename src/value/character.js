import { CHARACTER } from './type';

export default class CharacterValue {
  constructor(value) {
    this.type = CHARACTER;
    this.value = value;
  }
  inspect() {
    if (this.value === '\0') return '#\\nul';
    if (this.value === '\n') return '#\\newline';
    if (this.value === '\r') return '#\\return';
    if (this.value === '\t') return '#\\tab';
    if (this.value === ' ') return '#\\space';
    return '#\\' + this.value;
  }
}
