import { BOOLEAN } from './type';

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

export default function getValue(value) {
  if (value) return TRUE;
  return FALSE;
}

getValue.TRUE = TRUE;
getValue.FALSE = FALSE;
