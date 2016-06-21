import ProcedureValue from '../value/procedure';
import RealValue from '../value/number';
import BooleanValue from '../value/boolean';
import { SYMBOL, BOOLEAN } from '../value/value';

export function define(stack) {
  switch (stack.procTrack) {
  case 0:
    stack.buffer.name = stack.expression.cdr.car;
    if (stack.buffer.name.type !== SYMBOL) {
      throw new Error('Symbol expected, ' + stack.buffer.name.inspect() +
        ' received');
    }
    this.pushStack(stack.expression.cdr.cdr.car);
    break;
  case 1:
    this.rootParameters[stack.buffer.name.value] = stack.result;
    stack.result = undefined;
    return true;
  }
}

define.variable = 'define';

export function lambda(stack) {
  stack.result = new ProcedureValue('_lambda_', stack.expression.cdr.car,
    stack.expression.cdr.cdr, stack.scope);
  return true;
}

lambda.variable = 'lambda';

export function quote(stack) {
  stack.result = stack.expTrack.car;
  return true;
}

quote.variable = 'quote';

export function add(stack) {
  if (stack.procTrack === 0) {
    stack.buffer.sum = 0;
  } else {
    // TODO Type check!
    stack.buffer.sum += stack.result.value;
  }
  if (stack.expTrack) {
    this.pushStack(stack.expTrack.car);
    stack.expTrack = stack.expTrack.cdr;
  } else {
    stack.result = new RealValue(stack.buffer.sum);
    return true;
  }
}

add.variable = '+';

export function subtract(stack) {
  if (stack.procTrack === 1) {
    stack.buffer.sum = stack.result.value;
  } else if (stack.procTrack !== 0) {
    // TODO Type check!
    stack.buffer.sum -= stack.result.value;
  }
  if (stack.expTrack) {
    this.pushStack(stack.expTrack.car);
    stack.expTrack = stack.expTrack.cdr;
  } else {
    stack.result = new RealValue(stack.buffer.sum);
    return true;
  }
}

subtract.variable = '-';

export function ifFunc(stack) {
  switch (stack.procTrack) {
  case 0:
    this.pushStack(stack.expTrack.car);
    stack.expTrack = stack.expTrack.cdr;
    break;
  case 1:
    if (stack.result.type === BOOLEAN && stack.result.value === true) {
      // Follow consequent!
      this.jumpStack(stack.expTrack.car);
      return true;
    } else {
      // Follow alternate!
      if (stack.expTrack.cdr == null) {
        // Unspecified behavior.. What should I do?
        stack.result = undefined;
        return true;
      } else {
        this.jumpStack(stack.expTrack.cdr.car);
        return true;
      }
    }
  }
}

ifFunc.variable = 'if';

// Because I hate WCDMA?
export function lteFunc(stack) {
  switch (stack.procTrack) {
  case 0:
    this.pushStack(stack.expTrack.car);
    stack.expTrack = stack.expTrack.cdr;
    break;
  case 1:
    stack.buffer.a = stack.result;
    this.pushStack(stack.expTrack.car);
    break;
  case 2:
    stack.buffer.b = stack.result;
    // Done. :P
    // TODO Type check
    stack.result =
      new BooleanValue(stack.buffer.a.value <= stack.buffer.b.value);
    return true;
  }
}

lteFunc.variable = '<=';

export function display(stack) {
  if (stack.procTrack === 0) {
    this.pushStack(stack.expTrack.car);
  } else {
    // TODO Display function shouldn't add line break....
    console.log(stack.result.value);
    stack.result = undefined;
    return true;
  }
}

display.variable = 'display';
