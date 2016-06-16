import { NUMBER } from './value';

export const COMPLEX = Symbol('complex');
export const RATIONAL = Symbol('rational');

export default class RationalValue {
  constructor(value) {
    this.type = NUMBER;
    this.subType = RATIONAL;
    this.value = value;
  }
  inspect() {
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
