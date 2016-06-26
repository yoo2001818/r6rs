import { BOOLEAN } from './index';

export class BooleanValue {
  constructor(value) {
    this.type = BOOLEAN;
    this.value = value;
  }
  inspect() {
    return this.value ? '#t' : '#f';
  }
}

export const TRUE = new BooleanValue(true);
export const FALSE = new BooleanValue(false);

BooleanValue.TRUE = TRUE;
BooleanValue.FALSE = FALSE;

export default function getValue(value) {
  if (value) return TRUE;
  return FALSE;
}
