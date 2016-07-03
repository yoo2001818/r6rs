import NativeProcedureValue from '../../value/nativeProcedure';
import CharacterValue from '../../value/character';
import StringValue from '../../value/string';
import BooleanValue from '../../value/boolean';
import { CHARACTER, STRING } from '../../value';

import assert from '../../util/assert';

import createComparator from '../util/createComparator';

import schemeCode from './unicode.scm';

export default [
  new NativeProcedureValue('char-upcase', list => {
    assert(list.car, 'character');
    return new CharacterValue(list.car.value.toUpperCase());
  }, ['char']),
  new NativeProcedureValue('char-downcase', list => {
    assert(list.car, 'character');
    return new CharacterValue(list.car.value.toLowerCase());
  }, ['char']),
  createComparator('char-ci=?', CHARACTER,
    (a, b) => a.toLowerCase() === b.toLowerCase()),
  createComparator('char-ci<?', CHARACTER,
    (a, b) => a.toLowerCase() < b.toLowerCase()),
  createComparator('char-ci>?', CHARACTER,
    (a, b) => a.toLowerCase() > b.toLowerCase()),
  createComparator('char-ci<=?', CHARACTER,
    (a, b) => a.toLowerCase() <= b.toLowerCase()),
  createComparator('char-ci>=?', CHARACTER,
    (a, b) => a.toLowerCase() >= b.toLowerCase()),
  new NativeProcedureValue('char-alphabetic?', list => {
    assert(list.car, 'character');
    return new BooleanValue(/^[a-zA-Z]+$/i.test(list.car));
  }, ['char']),
  new NativeProcedureValue('char-numeric?', list => {
    assert(list.car, 'character');
    return new BooleanValue(/^[0-9]+$/i.test(list.car));
  }, ['char']),
  new NativeProcedureValue('char-whitespace?', list => {
    assert(list.car, 'character');
    return new BooleanValue(/^\s+$/i.test(list.car));
  }, ['char']),
  new NativeProcedureValue('char-upper-case?', list => {
    assert(list.car, 'character');
    return new BooleanValue(list.car.toUpperCase() === list.car &&
      list.car.toLowerCase() !== list.car);
  }, ['char']),
  new NativeProcedureValue('char-lower-case?', list => {
    assert(list.car, 'character');
    return new BooleanValue(list.car.toUpperCase() !== list.car &&
      list.car.toLowerCase() === list.car);
  }, ['char']),
  // Implementing char-general-category is totally unnecessary.
  new NativeProcedureValue('string-upcase', list => {
    assert(list.car, 'string');
    return new StringValue(list.car.value.toUpperCase());
  }, ['string']),
  new NativeProcedureValue('string-downcase', list => {
    assert(list.car, 'string');
    return new StringValue(list.car.value.toLowerCase());
  }, ['string']),
  createComparator('string-ci=?', STRING,
    (a, b) => a.toLowerCase() === b.toLowerCase()),
  createComparator('string-ci<?', STRING,
    (a, b) => a.toLowerCase() < b.toLowerCase()),
  createComparator('string-ci>?', STRING,
    (a, b) => a.toLowerCase() > b.toLowerCase()),
  createComparator('string-ci<=?', STRING,
    (a, b) => a.toLowerCase() <= b.toLowerCase()),
  createComparator('string-ci>=?', STRING,
    (a, b) => a.toLowerCase() >= b.toLowerCase()),
  new NativeProcedureValue('string-normalize-nfd', list => {
    assert(list.car, 'string');
    return new StringValue(list.car.value.normalize('NFD'));
  }, ['string']),
  new NativeProcedureValue('string-normalize-nfkd', list => {
    assert(list.car, 'string');
    return new StringValue(list.car.value.normalize('NFKD'));
  }, ['string']),
  new NativeProcedureValue('string-normalize-nfc', list => {
    assert(list.car, 'string');
    return new StringValue(list.car.value.normalize('NFC'));
  }, ['string']),
  new NativeProcedureValue('string-normalize-nfkc', list => {
    assert(list.car, 'string');
    return new StringValue(list.car.value.normalize('NFKC'));
  }, ['string']),
  schemeCode
];
