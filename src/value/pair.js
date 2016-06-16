import { PAIR } from './value';

export default class PairValue {
  constructor(car, cdr) {
    this.type = PAIR;
    this.car = car;
    this.cdr = cdr;
  }
  length() {
    let n = 0;
    let node = this;
    while (node != null && node.type === PAIR) {
      n ++;
      node = node.cdr;
    }
    return n;
  }
  forEach(callback, thisArg) {
    let n = 0;
    let node = this;
    while (node != null && node.type === PAIR) {
      callback.call(thisArg, node.car, n, this);
      n ++;
      node = node.cdr;
    }
    if (node != null) {
      callback.call(thisArg, node, n, this);
    }
  }
  every(callback, thisArg) {
    let n = 0;
    let node = this;
    while (node != null && node.type === PAIR) {
      if (!callback.call(thisArg, node.car, n, this)) return false;
      n ++;
      node = node.cdr;
    }
    if (node != null) {
      if (!callback.call(thisArg, node, n, this)) return false;
    }
    return true;
  }
  some(callback, thisArg) {
    let n = 0;
    let node = this;
    while (node != null && node.type === PAIR) {
      if (callback.call(thisArg, node.car, n, this)) return true;
      n ++;
      node = node.cdr;
    }
    if (node != null) {
      if (callback.call(thisArg, node, n, this)) return true;
    }
    return false;
  }
  filter(callback, thisArg) {
    let parent = null;
    let rootNode = parent;
    let node = this;
    let n = 0;
    while (node != null && node.type === PAIR) {
      if (callback.call(thisArg, node.car, n, this)) {
        let o = new PairValue(node.car, null);
        if (parent) parent.cdr = o;
        parent = o;
      }
      n ++;
      node = node.cdr;
    }
    if (node != null) {
      if (callback.call(thisArg, node, n, this)) {
        if (parent) parent.cdr = node;
      }
    }
    return rootNode;
  }
  map(callback, thisArg) {
    let parent = null;
    let rootNode = parent;
    let node = this;
    let n = 0;
    while (node != null && node.type === PAIR) {
      let o = new PairValue(callback.call(thisArg, node.car, n, this), null);
      if (parent) parent.cdr = o;
      parent = o;
      n ++;
      node = node.cdr;
    }
    if (node != null) {
      if (parent) parent.cdr = callback.call(thisArg, node, n, this);
    }
    return rootNode;
  }
  concat(target) {
    let parent = null;
    let rootNode = parent;
    let node = this;
    while (node != null && node.type === PAIR) {
      let o = new PairValue(node.car, null);
      if (parent) parent.cdr = o;
      parent = o;
      node = node.cdr;
    }
    if (node != null) {
      let o = new PairValue(node, null);
      if (parent) parent.cdr = o;
      parent = o;
    }
    node = target;
    while (node != null && node.type === PAIR) {
      let o = new PairValue(node.car, null);
      if (parent) parent.cdr = o;
      parent = o;
      node = node.cdr;
    }
    if (node != null) {
      if (parent) parent.cdr = node;
    }
    return rootNode;
  }
  toArray() {
    let output = [];
    let node = this;
    while (node != null && node.type === PAIR) {
      if (node.car) output.push(node.car.inspect());
      node = node.cdr;
    }
    if (node != null) {
      output.push(node.cdr.inspect());
    }
    return output;
  }
  inspect() {
    let output = [];
    let node = this;
    while (node != null && node.type === PAIR) {
      if (node.car) output.push(node.car.inspect());
      node = node.cdr;
    }
    if (node != null) {
      output.push('.');
      output.push(node.cdr.inspect());
    }
    return '(' + output.join(' ') + ')';
  }
  static fromArray(array) {
    let parent = null;
    let rootNode = parent;
    for (let i = 0; i < array.length; ++i) {
      let o = new PairValue(array[i], null);
      if (parent) parent.cdr = o;
      parent = o;
    }
    return rootNode;
  }
}
