import NativeProcedureValue from '../value/nativeProcedure';
import SymbolValue from '../value/symbol';
import StringValue from '../value/string';
import BooleanValue from '../value/boolean';
import { SYMBOL } from '../value/value';

export default [
  new NativeProcedureValue('symbol?', list => {
    return new BooleanValue(list.car && list.car.type === SYMBOL);
  }),
  new NativeProcedureValue('symbol->string', list => {
    // Assert symbol
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
    // Assert string
    return new SymbolValue(list.car.value);
  })
];
