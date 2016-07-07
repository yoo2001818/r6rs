import NativeProcedureValue from '../../value/nativeProcedure';
import BooleanValue from '../../value/boolean';

import { CHARACTER, STRING } from '../../value';

import schemeCode from './port.scm';

export default [
  new NativeProcedureValue('write-char', (list, machine) => {
    // In order to allow implementing display function with Scheme, we
    // allow strings in write-char too
    if (list.car == null || (list.car.type !== STRING && list.car.type !==
      CHARACTER)
    ) {
      throw new Error('char must be String or Character, ' +
        list.car.inspect() + 'received');
    }
    machine.stdout(list.car.value);
    return BooleanValue.TRUE;
  }, ['char']),
  new NativeProcedureValue('write', (list, machine) => {
    machine.stdout(list.car.inspect().toString());
    return BooleanValue.TRUE;
  }, ['obj']),
  schemeCode
];
