import NativeProcedureValue from '../value/nativeProcedure';
import BooleanValue from '../value/boolean';
import { BOOLEAN } from '../value/value';

export default [
  new NativeProcedureValue('not', list => {
    let val = list.car;
    return new BooleanValue(val && val.type === BOOLEAN && val.value === false);
  }),
  new NativeProcedureValue('boolean?', list => {
    return new BooleanValue(list.car && list.car.type === BOOLEAN);
  }),
  new NativeProcedureValue('boolean=?', list => {
    if (!list.car || list.car.type !== BOOLEAN) return new BooleanValue(false);
    let first = list.car.value;
    return new BooleanValue(list.every(
      v => v.type === BOOLEAN && v.value === first
    ));
  })
];
