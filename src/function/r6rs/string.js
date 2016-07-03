import NativeProcedureValue from '../../value/nativeProcedure';
import CharacterValue from '../../value/character';
import StringValue from '../../value/string';
import RealValue from '../../value/number';
import BooleanValue from '../../value/boolean';
import SymbolValue from '../../value/symbol';
import PairValue from '../../value/pair';
import { STRING } from '../../value';

import assert from '../../util/assert';

import createComparator from '../util/createComparator';

function stringToList(string) {
  let head, tail;
  // Assert string
  for (let i = 0; i < string.length; ++i) {
    let pair = new PairValue(new CharacterValue(string[i]));
    if (head == null) {
      head = pair;
      tail = pair;
    } else {
      tail.cdr = pair;
      tail = pair;
    }
  }
  return head || new PairValue();
}

export default [
  new NativeProcedureValue('string?', list => {
    return new BooleanValue(list.car && list.car.type === STRING);
  }, ['obj']),
  new NativeProcedureValue('make-string', list => {
    assert(list.car, 'number');
    assert(list.cdr.car, 'character');
    let letters = list.car.value;
    let char = (list.cdr && list.cdr.car.value) || '\0';
    let output = '';
    for (let i = 0; i < letters; ++i) {
      output += char;
    }
    return new StringValue(output);
  }, ['k', 'char']),
  new NativeProcedureValue('string', list => {
    let output = '';
    list.forEach(char => {
      assert(char, 'character');
      output += char.value;
    });
    return new StringValue(output);
  }, null, 'char'),
  new NativeProcedureValue('string-length', list => {
    assert(list.car, 'string');
    return new RealValue(list.car.value.length);
  }, ['string']),
  new NativeProcedureValue('string-ref', list => {
    // Assert string, integer, range
    assert(list.car, 'string');
    assert(list.cdr.car, 'number');
    return new CharacterValue(list.car.value[list.cdr.car.value]);
  }, ['string', 'k']),
  createComparator('string=?', STRING, (a, b) => a === b),
  createComparator('string<?', STRING, (a, b) => a < b),
  createComparator('string>?', STRING, (a, b) => a > b),
  createComparator('string<=?', STRING, (a, b) => a <= b),
  createComparator('string>=?', STRING, (a, b) => a >= b),
  new NativeProcedureValue('substring', list => {
    // Assert string, integer, integer
    assert(list.car, 'string');
    assert(list.cdr.car, 'number');
    assert(list.cdr.cdr.car, 'number');
    let original = list.car.value;
    let start = list.cdr.car.value;
    let end = list.cdr.cdr.car.value;
    return new StringValue(original.slice(start, end));
  }, ['string', 'start', 'end']),
  new NativeProcedureValue('string-append', list => {
    let output = '';
    list.forEach(string => {
      // Assert string
      assert(string, 'string');
      output += string.value;
    });
    return new StringValue(output);
  }, null, 'string'),
  new NativeProcedureValue('string->list', list => {
    assert(list.car, 'string');
    return stringToList(list.car.value);
  }, ['string']),
  new NativeProcedureValue('list->string', list => {
    let output = '';
    assert(list.car, 'pair');
    list.car.forEach(char => {
      assert(char, 'character');
      output += char.value;
    });
    return new StringValue(output);
  }, ['list']),
  new NativeProcedureValue('string-for-each', (list, machine, frame) => {
    // All this does is convert the request to for-each request. Easy!
    if (frame.procTrack === 0) {
      let request = new PairValue(new SymbolValue('for-each'),
        new PairValue(list.car, list.cdr.map(v => {
          assert(v, 'string');
          return new PairValue(new SymbolValue('quote'),
            new PairValue(stringToList(v.value)));
        })));
      machine.pushStack(request);
    } else {
      return frame.result;
    }
  }, ['proc'], 'string'),
  new NativeProcedureValue('string-copy', list => {
    return new StringValue(list.car.value);
  }, ['string'])
];
