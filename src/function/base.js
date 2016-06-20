import ProcedureValue from '../value/procedure';
import { SYMBOL } from '../value/value';

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
