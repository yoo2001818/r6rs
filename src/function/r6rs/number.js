import NativeProcedureValue from '../../value/nativeProcedure';
import RealValue from '../../value/number';
import BooleanValue from '../../value/boolean';
import StringValue from '../../value/string';
import { NUMBER } from '../../value';

import createComparator from '../util/createComparator';
import schemeCode from './number.scm';

import assert from '../../util/assert';

function gcd(a, b) {
  if (a < 0) return gcd(-a, b);
  if (b < 0) return gcd(a, -b);
  if (b > a) return gcd(b, a);
  while (a !== 0 && b !== 0) {
    if (b === 0) return a;
    a %= b;
    if (a === 0) return b;
    b %= a;
  }
}

function lcm(a, b) {
  return (a * b) / gcd(a, b);
}

function createNumberTest(name, test) {
  return new NativeProcedureValue(name, list => {
    return new BooleanValue(list.car && list.car.type === NUMBER &&
      test(list.car.value));
  });
}

function createNumberCalc(name, callback, single) {
  return new NativeProcedureValue(name, list => {
    if (list.cdr == null && single) {
      assert(list.car, 'number');
      return new RealValue(single(list.car.value));
    }
    return new RealValue(list.reduce((a, current) => {
      let prev = a;
      if (a.type === NUMBER) prev = a.value;
      if (!current || current.type !== NUMBER) return prev;
      return callback(prev, current.value);
    }));
  });
}

function createNumberOne(name, callback) {
  return new NativeProcedureValue(name, list => {
    assert(list.car, 'number');
    return new RealValue(callback(list.car.value));
  });
}

function createNumberTwo(name, callback) {
  return new NativeProcedureValue(name, list => {
    assert(list.car, 'number');
    return new RealValue(callback(list.car.value, list.cdr.car.value));
  });
}

export default [
  createNumberTest('number?', () => true),
  // TODO Complex is not supported yet
  new NativeProcedureValue('complex?', () => {
    return new BooleanValue(true);
  }),
  createNumberTest('real?', () => true),
  // Treat all number as rational number, for now.
  createNumberTest('rational?', () => true),
  createNumberTest('integer?', a => ((a % 1) === 0)),
  // We don't have rational number support yet.. so just check for
  // the integers.
  createNumberTest('exact?', a => ((a % 1) === 0)),
  createNumberTest('inexact?', a => ((a % 1) !== 0)),
  createComparator('=', NUMBER, (a, b) => a === b),
  createComparator('<', NUMBER, (a, b) => a < b),
  createComparator('>', NUMBER, (a, b) => a > b),
  createComparator('<=', NUMBER, (a, b) => a <= b),
  createComparator('>=', NUMBER, (a, b) => a >= b),
  createNumberTest('zero?', a => (a === 0)),
  createNumberTest('positive?', a => (a > 0)),
  createNumberTest('negative?', a => (a < 0)),
  createNumberTest('odd?', a => ((a % 2) === 1)),
  createNumberTest('even?', a => ((a % 2) === 0)),
  createNumberTest('finite?', a => Number.isFinite(a)),
  createNumberTest('infinite?', a => !Number.isFinite(a)),
  createNumberCalc('max', (a, b) => Math.max(a, b)),
  createNumberCalc('min', (a, b) => Math.min(a, b)),
  createNumberCalc('+', (a, b) => a + b),
  createNumberCalc('-', (a, b) => a - b, a => -a),
  createNumberCalc('/', (a, b) => a / b, a => 1 / a),
  createNumberCalc('*', (a, b) => a * b),
  createNumberOne('abs', a => Math.abs(a)),
  createNumberCalc('div', (a, b) => Math.floor(a / b)),
  createNumberCalc('mod', (a, b) => Math.floor(a % b)),
  createNumberCalc('div0', (a, b) => (a / b | 0)),
  createNumberCalc('mod0', (a, b) => (a % b | 0)),
  createNumberCalc('gcd', (a, b) => gcd(a, b)),
  createNumberCalc('lcm', (a, b) => lcm(a, b)),
  // TODO implement numberator, denominator
  createNumberOne('floor', a => Math.floor(a)),
  createNumberOne('ceiling', a => Math.ceiling(a)),
  createNumberOne('truncate', a => a | 0),
  createNumberOne('round', a => Math.round(a)),
  // TODO implement rationalize
  createNumberOne('exp', a => Math.exp(a)),
  new NativeProcedureValue('log', list => {
    assert(list.car, 'number');
    if (list.cdr == null) {
      return new RealValue(Math.log(list.car.value));
    } else {
      assert(list.cdr, 'number');
      return new RealValue(Math.log(list.car.value) /
        Math.log(list.cdr.car.value));
    }
  }),
  createNumberOne('sin', a => Math.sin(a)),
  createNumberOne('cos', a => Math.cos(a)),
  createNumberOne('tan', a => Math.tan(a)),
  createNumberOne('asin', a => Math.asin(a)),
  createNumberOne('acos', a => Math.acos(a)),
  new NativeProcedureValue('atan', list => {
    assert(list.car, 'number');
    if (list.cdr == null) {
      return new RealValue(Math.atan(list.car.value));
    } else {
      assert(list.cdr, 'number');
      return new RealValue(Math.atan2(list.car.value, list.cdr.car.value));
    }
  }),
  createNumberOne('sqrt', a => Math.sqrt(a)),
  // TODO exact-integer-sqrt
  createNumberTwo('expt', (a, b) => Math.pow(a, b)),
  // TODO make-rectangular, make-polar, real-part, imag-part, magnitude, angle
  // these require complex numbers (which is going to be quite complex :P)
  new NativeProcedureValue('number->string', list => {
    // Ignore precision for now
    assert(list.car, 'number');
    if (list.car.value === Infinity) return new StringValue('inf.0');
    if (list.car.value === -Infinity) return new StringValue('-inf.0');
    if (isNaN(list.car.value)) return new StringValue('nan.0');
    return new StringValue((list.car.value).toString(list.cdr.car.value));
  }),
  new NativeProcedureValue('string->number', list => {
    assert(list.car, 'string');
    if (list.car.value === 'inf.0' || list.car.value === '+inf.0') {
      return new RealValue(Infinity);
    }
    if (list.car.value === '-inf.0') {
      return new RealValue(-Infinity);
    }
    if (/^[\-+]?nan.0$/.test(list.car.value)) {
      return new RealValue(NaN);
    }
    return new RealValue(parseFloat(list.car.value, list.cdr.car.value));
  }),
  schemeCode
];
