import NativeProcedureValue from '../value/nativeProcedure';
import CharacterValue from '../value/character';
import RealValue from '../value/number';
import BooleanValue from '../value/boolean';
import { CHARACTER } from '../value/value';

import createComparator from './util/createComparator';

export default [
  new NativeProcedureValue('char?', list => {
    return new BooleanValue(list.car && list.car.type === CHARACTER);
  }),
  // code point supports UTF-16 surrogate chars, however some browsers may
  // not support it.
  new NativeProcedureValue('char->integer', list => {
    // Assert char
    return new RealValue(list.car.value.codePointAt(0));
  }),
  new NativeProcedureValue('integer->char', list => {
    // Assert integer
    return new CharacterValue(String.fromCodePoint(list.car.value));
  }),
  createComparator('char=?', CHARACTER, (a, b) => a === b),
  createComparator('char<?', CHARACTER, (a, b) => a < b),
  createComparator('char>?', CHARACTER, (a, b) => a > b),
  createComparator('char<=?', CHARACTER, (a, b) => a <= b),
  createComparator('char>=?', CHARACTER, (a, b) => a >= b)
];
