import NativeProcedureValue from '../../value/nativeProcedure';
import CharacterValue from '../../value/character';
import RealValue from '../../value/number';
import BooleanValue from '../../value/boolean';
import { CHARACTER } from '../../value';

import createComparator from '../util/createComparator';
import assert from '../../util/assert';

export default [
  new NativeProcedureValue('char?', list => {
    return new BooleanValue(list.car && list.car.type === CHARACTER);
  }, ['obj']),
  // code point supports UTF-16 surrogate chars, however some browsers may
  // not support it.
  new NativeProcedureValue('char->integer', list => {
    assert(list.car, 'character');
    return new RealValue(list.car.value.codePointAt(0));
  }, ['char']),
  new NativeProcedureValue('integer->char', list => {
    assert(list.car, 'number');
    return new CharacterValue(String.fromCodePoint(list.car.value));
  }, ['integer']),
  createComparator('char=?', CHARACTER, (a, b) => a === b),
  createComparator('char<?', CHARACTER, (a, b) => a < b),
  createComparator('char>?', CHARACTER, (a, b) => a > b),
  createComparator('char<=?', CHARACTER, (a, b) => a <= b),
  createComparator('char>=?', CHARACTER, (a, b) => a >= b)
];
