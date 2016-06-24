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
  new NativeProcedureValue('map', (list, machine, frame) => {
    let procedure = list.car;
    // Assert procedure
    let argsHeader = new PairValue(procedure);
    let argsTail = argsHeader;

    if (frame.bufferHead == null) {
      frame.bufferHead = list.cdr.copy();
      frame.result = null;
    }
    if (frame.result != null) {
      let pair = new PairValue(frame.result);
      if (frame.bufferOutputHead == null) {
        frame.bufferOutputHead = pair;
        frame.bufferOutputTail = pair;
      } else {
        frame.bufferOutputTail.cdr = pair;
        frame.bufferOutputTail = pair;
      }
      frame.result = null;
    }
    let headHead = frame.bufferHead;
    let headNode = null;
    if (headHead.car != null) {
      headNode = headHead;
      // Visit every node, and increment each node.
      while (headNode != null && headNode.type === PAIR) {
        if (headNode.car == null || headNode.car.type !== PAIR) {
          throw new Error('List sizes does not match');
        }
        let argsPair = new PairValue(headNode.car.car);
        argsTail.cdr = argsPair;
        argsTail = argsPair;

        headNode.car = headNode.car.cdr;
        headNode = headNode.cdr;
      }

      // Push the stack frame...
      machine.pushStack(argsHeader);
      return undefined;
    }
    // Done!
    return frame.bufferOutputHead;
  }),
  new NativeProcedureValue('for-each', (list, machine, frame) => {
    // R6RS standard requires that more than one list should be handled.
    // This can be implemented kinda absurdly....
    let procedure = list.car;
    // Assert procedure
    let argsHeader = new PairValue(procedure);
    let argsTail = argsHeader;

    if (frame.bufferHead == null) frame.bufferHead = list.cdr.copy();
    let headHead = frame.bufferHead;
    let headNode = null;
    if (headHead.car != null) {
      headNode = headHead;
      // Visit every node, and increment each node.
      while (headNode != null && headNode.type === PAIR) {
        if (headNode.car == null || headNode.car.type !== PAIR) {
          throw new Error('List sizes does not match');
        }
        let argsPair = new PairValue(headNode.car.car);
        argsTail.cdr = argsPair;
        argsTail = argsPair;

        headNode.car = headNode.car.cdr;
        headNode = headNode.cdr;
      }

      // Push the stack frame...
      machine.pushStack(argsHeader);
      return undefined;
    }
    // Done!
    return new PairValue();
  }),
  schemeCode
];
