import NativeProcedureValue from '../../value/nativeProcedure';
import BooleanValue from '../../value/boolean';
import { BOOLEAN } from '../../value';

import createComparator from '../util/createComparator';

export default [
  new NativeProcedureValue('not', list => {
    let val = list.car;
    return new BooleanValue(val && val.type === BOOLEAN && val.value === false);
  }, ['obj']),
  new NativeProcedureValue('boolean?', list => {
    return new BooleanValue(list.car && list.car.type === BOOLEAN);
  }, ['obj']),
  createComparator('boolean=?', BOOLEAN, (a, b) => a === b)
];
