import NativeProcedureValue from '../value/nativeProcedure';
import BooleanValue, { TRUE, FALSE } from '../value/boolean';
import PairValue from '../value/pair';
import { PAIR } from '../value/value';

import schemeCode from './pair.scm';

export default [
  new NativeProcedureValue('pair?', list => {
    return new BooleanValue(list.car && list.car.type === PAIR);
  }),
  new NativeProcedureValue('null?', list => {
    if (list.car == null) return TRUE;
    if (list.car.type !== PAIR) return FALSE;
    return new BooleanValue(list.car.car == null && list.car.cdr == null);
  }),
  new NativeProcedureValue('list?', list => {
    return new BooleanValue(list.car && list.car.isList());
  }),
  new NativeProcedureValue('cons', list => {
    return new PairValue(list.car, list.cdr.car);
  }),
  new NativeProcedureValue('car', list => {
    if (list.car.car == null) throw new Error('Assertion exception');
    return list.car.car;
  }),
  new NativeProcedureValue('cdr', list => {
    if (list.car.cdr == null) throw new Error('Assertion exception');
    return list.car.cdr;
  }),
  new NativeProcedureValue('list', list => {
    return list;
  }),
  new NativeProcedureValue('length', list => {
    // assert pair
    return list.car.length();
  }),
  new NativeProcedureValue('append', list => {
    let head, tail;
    let outerNode = list;
    while (outerNode != null && outerNode.type === PAIR) {
      let node = outerNode.car;
      outerNode = outerNode.cdr;
      while (node != null && node.type === PAIR) {
        let o = new PairValue(node.car, null);
        if (tail) {
          tail.cdr = o;
          tail = o;
        } else {
          head = o;
          tail = o;
        }
        node = node.cdr;
      }
      if (node != null) {
        if (outerNode == null) {
          if (tail) {
            tail.cdr = node;
          } else {
            // If no pairs are allocated, obj is returned.
            return node;
          }
        } else {
          let o = new PairValue(node, null);
          if (tail) {
            tail.cdr = o;
            tail = o;
          } else {
            head = o;
            tail = o;
          }
        }
      }
    }
    return head || new PairValue();
  }),
  new NativeProcedureValue('reverse', list => {
    // Assert list
    let output;
    let node = list.car;
    while (node != null && node.type === PAIR) {
      output = new PairValue(node.car, output);
      node = node.cdr;
    }
    return output || new PairValue();
  }),
  new NativeProcedureValue('map', list => {

  }),
  new NativeProcedureValue('for-each', list => {

  }),
  schemeCode
];
