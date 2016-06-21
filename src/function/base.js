import LambdaValue from '../value/lambda';
import NativeProcedureValue from '../value/nativeProcedure';
import NativeSyntaxValue from '../value/nativeSyntax';
import RealValue from '../value/number';
import BooleanValue from '../value/boolean';
import PairValue from '../value/pair';
import { SYMBOL, BOOLEAN } from '../value/value';

export const defineFunc = new NativeSyntaxValue('define', (machine, frame) => {
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
    machine.rootParameters[frame.bufferName.value] = frame.result;
    frame.result = new PairValue();
    return true;
  }
});

export const lambda = new NativeSyntaxValue('lambda', (machine, frame) => {
  frame.result = new LambdaValue('_lambda_', frame.expTrack.cdr,
    frame.expTrack.car, frame.scope);
  return true;
});

export const quote = new NativeSyntaxValue('quote', (machine, frame) => {
  frame.result = frame.expTrack.car;
  return true;
});

export const ifFunc = new NativeSyntaxValue('if', (machine, frame) => {
  switch (frame.procTrack) {
  case 0:
    machine.pushStack(frame.expTrack.car);
    frame.expTrack = frame.expTrack.cdr;
    break;
  case 1:
    if (frame.result.type === BOOLEAN && frame.result.value === true) {
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
});

export const add = new NativeProcedureValue('+', (_, list) => {
  let sum = 0;
  list.forEach(v => sum += v.value);
  return new RealValue(sum);
});

export const subtract = new NativeProcedureValue('-', (_, list) => {
  let sum = list.car.value;
  list.cdr.forEach(v => sum -= v.value);
  return new RealValue(sum);
});

// Because I hate WCDMA?
export const lteFunc = new NativeProcedureValue('<=', (machine, list) => {
  return new BooleanValue(list.car.value <= list.cdr.car.value);
});

export const display = new NativeProcedureValue('display', (machine, list) => {
  console.log(list.car);
  return null;
});
