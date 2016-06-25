import { PAIR } from './value';

export default class PairValue {
  constructor(car, cdr) {
    this.type = PAIR;
    this.car = car;
    this.cdr = cdr;
  }
  isEmpty() {
    return this.car == null && this.cdr == null;
  }
  isList() {
    let node = this;
    while (node != null && node.type === PAIR) {
      node = node.cdr;
    }
    return node === null;
  }
  length(cdr = true) {
    let n = 0;
    let node = this;
    while (node != null && node.type === PAIR) {
      n ++;
      node = node.cdr;
    }
    if (node != null && cdr) n ++;
    return n;
  }
  forEach(callback, thisArg) {
    let n = 0;
    let node = this;
    while (node != null && node.type === PAIR) {
      if (node.car) {
        callback.call(thisArg, node.car, n, node);
      }
      n ++;
      node = node.cdr;
    }
    if (node != null) {
      callback.call(thisArg, node, n, null);
    }
  }
  every(callback, thisArg) {
    let n = 0;
    let node = this;
    while (node != null && node.type === PAIR) {
      if (node.car) {
        if (!callback.call(thisArg, node.car, n, node)) return false;
      }
      n ++;
      node = node.cdr;
    }
    if (node != null) {
      if (!callback.call(thisArg, node, n, null)) return false;
    }
    return true;
  }
  some(callback, thisArg) {
    let n = 0;
    let node = this;
    while (node != null && node.type === PAIR) {
      if (node.car) {
        if (callback.call(thisArg, node.car, n, node)) return true;
      }
      n ++;
      node = node.cdr;
    }
    if (node != null) {
      if (callback.call(thisArg, node, n, null)) return true;
    }
    return false;
  }
  filter(callback, thisArg) {
    let parent, rootNode;
    let node = this;
    let n = 0;
    while (node != null && node.type === PAIR) {
      if (node.car && callback.call(thisArg, node.car, n, node)) {
        let o = new PairValue(node.car, null);
        if (parent) {
          parent.cdr = o;
          parent = o;
        } else {
          parent = o;
          rootNode = o;
        }
      }
      n ++;
      node = node.cdr;
    }
    if (node != null) {
      if (callback.call(thisArg, node, n, null)) {
        if (parent) parent.cdr = node;
      }
    }
    return rootNode || new PairValue();
  }
  map(callback, thisArg) {
    let parent, rootNode;
    let node = this;
    let n = 0;
    while (node != null && node.type === PAIR) {
      let o;
      if (node.car == null) {
        o = new PairValue();
      } else {
        o = new PairValue(callback.call(thisArg, node.car, n, node), null);
      }
      if (parent) {
        parent.cdr = o;
        parent = o;
      } else {
        parent = o;
        rootNode = o;
      }
      n ++;
      node = node.cdr;
    }
    if (node != null) {
      if (parent) parent.cdr = callback.call(thisArg, node, n, null);
    }
    return rootNode;
  }
  concat(target) {
    let parent, rootNode;
    let node = this;
    while (node != null && node.type === PAIR) {
      let o = new PairValue(node.car, null);
      if (parent) {
        parent.cdr = o;
        parent = o;
      } else {
        parent = o;
        rootNode = o;
      }
      node = node.cdr;
    }
    if (node != null) {
      let o = new PairValue(node, null);
      if (parent) {
        parent.cdr = o;
        parent = o;
      } else {
        parent = o;
        rootNode = o;
      }
    }
    node = target;
    while (node != null && node.type === PAIR) {
      let o = new PairValue(node.car, null);
      if (parent) {
        parent.cdr = o;
        parent = o;
      } else {
        parent = o;
        rootNode = o;
      }
      node = node.cdr;
    }
    if (node != null) {
      if (parent) parent.cdr = node;
    }
    return rootNode;
  }
  // Create deep copy of the list.
  copy() {
    let parent, rootNode;
    let node = this;
    let n = 0;
    while (node != null && node.type === PAIR) {
      if (node.car) {
        let o = new PairValue(node.car, null);
        if (parent) {
          parent.cdr = o;
          parent = o;
        } else {
          parent = o;
          rootNode = o;
        }
      }
      n ++;
      node = node.cdr;
    }
    if (node != null) {
      if (parent) parent.cdr = node;
    }
    return rootNode || new PairValue();
  }
  reduce(callback, initialValue) {
    let node = this;
    let currentValue = initialValue, currentIndex = 0;
    if (initialValue === undefined) {
      node = this.cdr;
      currentValue = this.car;
      currentIndex = 1;
      if (this.car == null) {
        throw new TypeError('Reduce of empty list with no initial value');
      }
    }
    while (node != null && node.type === PAIR) {
      if (node.car != null) {
        currentValue = callback(currentValue, node.car, currentIndex, node);
        currentIndex ++;
      }
      node = node.cdr;
    }
    if (node != null) {
      currentValue = callback(currentValue, node, currentIndex, null);
    }
    return currentValue;
  }
  toArray() {
    let output = [];
    let node = this;
    while (node != null && node.type === PAIR) {
      if (node.car != null) output.push(node.car);
      node = node.cdr;
    }
    if (node != null) {
      output.push(node);
    }
    return output;
  }
  inspect() {
    let output = [];
    let node = this;
    while (node != null && node.type === PAIR) {
      if (node.car != null) {
        output.push(node.car.inspect ? node.car.inspect() : node.car);
      }
      node = node.cdr;
    }
    if (node != null) {
      output.push('.');
      output.push(node.inspect ? node.inspect() : node);
    }
    return '(' + output.join(' ') + ')';
  }
  static fromArray(array) {
    let parent = null;
    let rootNode = null;
    for (let i = 0; i < array.length; ++i) {
      let o = new PairValue(array[i], null);
      if (parent) {
        parent.cdr = o;
        parent = o;
      } else {
        parent = o;
        rootNode = o;
      }
    }
    return rootNode;
  }
}
