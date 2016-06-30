import NativeProcedureValue from '../value/nativeProcedure';
import SymbolValue from '../value/symbol';
import StringValue from '../value/string';
import BooleanValue from '../value/boolean';
import { SYMBOL } from '../value';

import assert from '../util/assert';

export default [
  new NativeProcedureValue('symbol?', list => {
    return new BooleanValue(list.car && list.car.type === SYMBOL);
  }),
  new NativeProcedureValue('symbol->string', list => {
    assert(list.car, 'symbol');
    return new StringValue(list.car.value);
  }),
  new NativeProcedureValue('symbol=?', list => {
    if (!list.car || list.car.type !== SYMBOL) return new BooleanValue(false);
    let first = list.car.value;
    return new BooleanValue(list.every(
      v => v.type === SYMBOL && v.value === first
    ));
  }),
  new NativeProcedureValue('string->symbol', list => {
    assert(list.car, 'string');
    return new SymbolValue(list.car.value);
  })
];
