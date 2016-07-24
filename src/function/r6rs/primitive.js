import LambdaValue from '../../value/lambda';
import NativeProcedureValue from '../../value/nativeProcedure';
import NativeSyntaxValue from '../../value/nativeSyntax';
import BooleanValue from '../../value/boolean';
import PairValue from '../../value/pair';
import { SYMBOL, BOOLEAN, NUMBER, CHARACTER, PAIR, STRING, PROCEDURE }
  from '../../value';

import schemeCode from './primitive.scm';
import pair from './pair';
import boolean from './boolean';
import symbol from './symbol';
import character from './character';
import string from './string';
import number from './number';

import assert from '../../util/assert';

// Base library - Primitive functions that depend on native calls.

export default [
  // Other forms will be handled by define-syntax. :)
  new NativeSyntaxValue('define', (machine, frame) => {
    switch (frame.procTrack) {
    case 0:
      frame.bufferName = frame.expTrack.car;
      if (frame.bufferName.type !== SYMBOL) {
        throw new Error('Symbol expected, ' + frame.bufferName.inspect() +
          ' received');
      }
      machine.pushStack(frame.expTrack.cdr.car);
      break;
    case 1:
      if (machine.libraryLevel) {
        machine.libraryParameters[frame.bufferName.value] = frame.result;
      } else {
        machine.rootParameters[frame.bufferName.value] = frame.result;
      }
      frame.result = new PairValue();
      return true;
    }
  }, ['variable', 'expression'], undefined, true),
  new NativeSyntaxValue('set!', (machine, frame) => {
    switch (frame.procTrack) {
    case 0:
      frame.bufferName = frame.expTrack.car;
      if (frame.bufferName.type !== SYMBOL) {
        throw new Error('Symbol expected, ' + frame.bufferName.inspect() +
          ' received');
      }
      machine.pushStack(frame.expTrack.cdr.car);
      break;
    case 1: {
      let name = frame.bufferName.value;
      let value = frame.result;
      frame.result = new PairValue();
      let node = frame.scope;
      while (node != null) {
        if (node.car[name] != null) {
          node.car[name] = value;
          return true;
        }
        node = node.cdr;
      }
      if (machine.rootParameters[name] != null) {
        machine.rootParameters[name] = value;
      }
      if (machine.libraryParameters[name] != null) {
        if (machine.libraryLevel) {
          machine.libraryParameters[name] = value;
        } else {
          machine.rootParameters[name] = value;
        }
      }
      return true;
    }}
  }, ['variable', 'expression'], undefined, true),
  // define-syntax is processed by expander, so machine itself doesn't have to
  // process it at all.
  new NativeSyntaxValue('define-syntax', () => {
    // NOP
    return true;
  }, ['keyword', 'expression']),
  new NativeSyntaxValue('let-syntax', () => {
    // NOP
    return true;
  }, ['keyword', 'expression']),
  new NativeSyntaxValue('letrec-syntax', () => {
    // NOP
    return true;
  }, ['keyword', 'expression']),
  new NativeSyntaxValue('lambda', (machine, frame) => {
    // If parent node is define or set! (at library level), we can pull the
    // name from previous stack and use that instead.
    let name = '_lambda_', nameValue;
    if (machine.stack.cdr && machine.stack.cdr.car) {
      let prevStack = machine.stack.cdr.car;
      if (prevStack.procedure === machine.libraryParameters['define'] ||
        prevStack.procedure === machine.libraryParameters['set!']
      ) {
        if (prevStack.expTrack.car && prevStack.expTrack.car.type === SYMBOL) {
          name = prevStack.expTrack.car.value;
          nameValue = prevStack.expTrack.car;
        }
      }
    }
    frame.result = new LambdaValue(name, frame.expTrack.cdr,
      frame.expTrack.car, frame.scope);
    frame.result.nameVal = nameValue;
    return true;
  }, ['formals'], 'body'),
  new NativeSyntaxValue('quote', (machine, frame) => {
    frame.result = frame.expTrack.car;
    return true;
  }, ['datum']),
  new NativeSyntaxValue('quasiquote', (machine, frame) => {
    if (frame.bufferStack == null) {
      // Initialize quasiquote traverser
      frame.bufferStack = new PairValue({
        input: frame.expTrack.car
      });
    }
    function pushValue(entry, value) {
      let pair = new PairValue(value);
      if (entry.outputTail) {
        entry.outputTail.cdr = pair;
        entry.outputTail = pair;
      } else {
        entry.outputHead = pair;
        entry.outputTail = pair;
      }
    }
    while (frame.bufferStack != null) {
      let entry = frame.bufferStack.car;
      if (entry.input == null) {
        frame.bufferStack = frame.bufferStack.cdr;
        if (frame.bufferStack != null) {
          frame.bufferStack.car.result = entry.outputHead || new PairValue();
        } else {
          frame.result = entry.outputHead || new PairValue();
          return true;
        }
      } else if (entry.input.type === PAIR) {
        if (entry.input.car.type === PAIR) {
          let subCode = entry.input.car;
          if (subCode.car.type === SYMBOL && subCode.car.value === 'unquote') {
            if (frame.result != null) {
              pushValue(entry, frame.result);
              frame.result = null;
            } else {
              machine.pushStack(subCode.cdr.car);
              return;
            }
          } else if (subCode.car.type === SYMBOL &&
            subCode.car.value === 'unquote-splicing'
          ) {
            if (frame.result != null) {
              let listNode = frame.result;
              while (listNode != null && listNode.type === PAIR) {
                pushValue(entry, listNode.car);
                listNode = listNode.cdr;
              }
              frame.result = null;
            } else {
              machine.pushStack(subCode.cdr.car);
              return;
            }
          } else {
            if (entry.result) {
              pushValue(entry, entry.result);
              entry.result = null;
            } else {
              frame.bufferStack = new PairValue({
                input: subCode
              }, frame.bufferStack);
              continue;
            }
          }
        } else {
          pushValue(entry, entry.input.car);
        }
        entry.input = entry.input.cdr;
      } else {
        // cdr value.
        if (entry.outputTail) {
          entry.outputTail.cdr = entry.input;
        } else {
          let pair = new PairValue(null, entry.input);
          entry.outputHead = entry.outputTail = pair;
        }
        entry.input = null;
      }
    }
    frame.result = frame.expTrack.car;
    return true;
  }, ['datum']),
  new NativeSyntaxValue('if', (machine, frame) => {
    switch (frame.procTrack) {
    case 0:
      machine.pushStack(frame.expTrack.car);
      frame.expTrack = frame.expTrack.cdr;
      break;
    case 1:
      if (frame.result.type !== BOOLEAN || frame.result.value === true) {
        // Follow consequent!
        machine.jumpStack(frame.expTrack.car);
        return true;
      } else {
        // Follow alternate!
        if (frame.expTrack.cdr == null) {
          // Unspecified behavior.. What should I do?
          frame.result = new BooleanValue(false);
          return true;
        } else {
          machine.jumpStack(frame.expTrack.cdr.car);
          return true;
        }
      }
    }
  }, ['test', 'consequent', 'alternate']),
  new NativeProcedureValue('begin', list => {
    let result = null;
    let node = list;
    while (node != null) {
      result = node.car;
      node = node.cdr;
    }
    return result;
  }, null, 'form'),
  new NativeProcedureValue('apply', (list, machine) => {
    assert(list.car, PROCEDURE);
    let listHead = new PairValue(list.car);
    let listTail = listHead;
    let node = list.cdr;
    while (node != null && node.cdr != null) {
      let pairValue = new PairValue(node.car);
      listTail.cdr = pairValue;
      listTail = pairValue;
      node = node.cdr;
    }
    assert(node.car, PAIR);
    listTail.cdr = node.car;
    machine.jumpStack(listHead, true);
    return true;
  }, ['proc'], 'args'),
  new NativeProcedureValue('error', list => {
    let output = 'Error: ';
    if (list.car && list.car.value !== false) {
      output += list.car.value + ': ';
    }
    output += list.cdr.car.value + ' ';
    output += list.cdr.cdr.inspect();
    throw new Error(output);
  }, ['who', 'message'], 'irritant'),
  new NativeProcedureValue('assertion-violation', list => {
    let output = 'Assertion violation: ';
    if (list.car && list.car.value !== false) {
      output += list.car.value + ': ';
    }
    output += list.cdr.car.value + ' ';
    output += list.cdr.cdr.inspect();
    throw new Error(output);
  }, ['who', 'message'], 'irritant'),
  new NativeProcedureValue('assert', (list, _, frame) => {
    let val = list.car;
    if (val && val.type === BOOLEAN && val.value === false) {
      // Assert if false
      throw new Error('Assertion violation: ' + frame.expression.cdr.inspect());
    }
    // Return if true
    return val;
  }, ['expression']),
  new NativeProcedureValue('eqv?', list => {
    let a = list.car;
    let b = list.cdr.car;
    if (a === b) return BooleanValue.TRUE;
    if (a.type !== b.type) return BooleanValue.FALSE;
    switch (a.type) {
    case BOOLEAN:
    case SYMBOL:
    case NUMBER:
    case CHARACTER:
      return new BooleanValue(a.value === b.value);
    case PAIR:
      if (a.isEmpty() && b.isEmpty()) return BooleanValue.TRUE;
      return new BooleanValue(a === b);
    default:
      return new BooleanValue(a === b);
    }
  }, ['obj1', 'obj2']),
  (() => {
    function checkEqual(a, b) {
      if (a == b) return true;
      if (a == null || b == null) return false;
      if (a.type !== b.type) return false;
      switch (a.type) {
      case BOOLEAN:
      case SYMBOL:
      case NUMBER:
      case CHARACTER:
      case STRING:
        return a.value === b.value;
      case PAIR:
        if (!checkEqual(a.car, b.car)) return false;
        if (!checkEqual(a.cdr, b.cdr)) return false;
        return true;
      default:
        return a === b;
      }
    }
    return new NativeProcedureValue('equal?', list => {
      let a = list.car;
      let b = list.cdr.car;
      return new BooleanValue(checkEqual(a, b));
    }, ['obj1', 'obj2']);
  })(),
  new NativeProcedureValue('procedure?', list => {
    return new BooleanValue(list.car && list.car.type === PROCEDURE);
  }, ['obj']),
  schemeCode,
  pair, boolean, symbol, character, string, number
];
