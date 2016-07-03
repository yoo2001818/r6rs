import NativeProcedureValue from '../../value/nativeProcedure';
import PairValue from '../../value/pair';
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
  schemeCode
];
