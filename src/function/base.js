import ConstValue from '../constValue';
import ProcedureValue from '../procedureValue';

export function define(machine, name, value) {
  machine.setVariable(name, machine.exec(value));
}

define.variable = 'define';

export function lambda(machine, args, ...codes) {
  return new ProcedureValue(args, codes);
}

lambda.variable = 'lambda';

export function constExpr(machine, a) {
  return new ConstValue(a);
}
constExpr.variable = 'const';

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
