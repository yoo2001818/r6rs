import NativeProcedureValue from '../../value/nativeProcedure';
import BooleanValue from '../../value/boolean';

export default function createComparator(name, type, comp) {
  return new NativeProcedureValue(name, list => {
    if (!list.car || list.car.type !== type) {
      return new BooleanValue(false);
    }
    let first = list.car.value;
    return new BooleanValue(list.cdr.every(v => {
      let result = v.type === type && comp(first, v.value);
      first = v.value;
      return result;
    }));
  });
}
