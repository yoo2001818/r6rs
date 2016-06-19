import ConstValue from '../constValue';
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
    console.log('set', stack.buffer.name.value, stack.result);
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

export function ifExpr(machine, cond, ifTrue, ifFalse) {
  let result = machine.exec(cond).value;
  if (result) {
    return machine.exec(ifTrue);
  } else {
    return machine.exec(ifFalse);
  }
}

ifExpr.variable = 'if';

export function lteExpr(machine, a, b) {
  let aVal = machine.exec(a).value;
  let bVal = machine.exec(b).value;
  console.log(aVal, bVal);
  return new ConstValue(aVal <= bVal);
}

lteExpr.variable = '<=';

export function add(machine, a, b) {
  let aVal = machine.exec(a).value;
  let bVal = machine.exec(b).value;
  return new ConstValue(aVal + bVal);
}

add.variable = '+';

export function subtract(machine, a, b) {
  let aVal = machine.exec(a).value;
  let bVal = machine.exec(b).value;
  return new ConstValue(aVal - bVal);
}

subtract.variable = '-';
