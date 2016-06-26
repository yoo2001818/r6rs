import { NUMBER } from './index';

export const COMPLEX = Symbol('complex');
export const REAL = Symbol('real');

export default class RealValue {
  constructor(value) {
    this.type = NUMBER;
    // this.subType = REAL;
    this.value = value;
  }
  inspect() {
    if (isNaN(this.value)) return 'nan.0';
    if (this.value === Infinity) return 'inf.0';
    if (this.value === -Infinity) return '-inf.0';
    return this.value;
  }
}

// I'm not sure why this is required though.
export class ComplexValue {
  constructor(real, imaginary) {
    this.type = NUMBER;
    this.subType = COMPLEX;
    this.value = real;
    this.imaginary = imaginary;
  }
  inspect() {
    if (this.imaginary > 0) {
      return this.value + ' + ' + this.imaginary + 'i';
    } else {
      return this.value + ' - ' + (-this.imaginary) + 'i';
    }
  }
}
