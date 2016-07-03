import NativeProcedureValue from '../../value/nativeProcedure';
import PairValue from '../../value/pair';

import { PROCEDURE, PAIR, BOOLEAN } from '../../value';
import assert from '../../util/assert';

import schemeCode from './list.scm';

export default [
  new NativeProcedureValue('cons*', list => {
    let listHead, listTail;
    let node = list;
    while (node != null && node.cdr != null) {
      let pairValue = new PairValue(node.car);
      if (listHead == null) {
        listHead = pairValue;
        listTail = pairValue;
      } else {
        listTail.cdr = pairValue;
        listTail = pairValue;
      }
      node = node.cdr;
    }
    if (listHead == null) {
      return node.car;
    }
    listTail.cdr = node.car;
    return listHead;
  }, null, 'args'),
  new NativeProcedureValue('list-sort', (list, machine) => {
    assert(list.car, PROCEDURE);
    assert(list.cdr.car, PAIR);
    // Just borrow native implementation
    let array = list.cdr.car.toArray();
    array.sort((a, b) => {
      machine.pushStack(new PairValue(
        list.car, new PairValue(a, new PairValue(b))
      ));
      let result = machine.execute();
      return (result.type != BOOLEAN || result.value === true) ? -1 : 1;
    });
    return PairValue.fromArray(array);
  }, ['proc', 'list']),
  schemeCode
];
