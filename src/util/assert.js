import * as Value from '../value';

const types = {};

for (let name in Value) {
  if (!/^[A-Z]+$/.test(name)) continue;
  types[name.toLowerCase()] = Value[name];
}

export default function assert(value, type) {
  let typeObj;
  if (typeof type === 'string') {
    typeObj = types[type];
  } else {
    typeObj = type;
  }
  if (value == null || value.type !== typeObj) {
    throw new Error('Assertion failed: ' + type + ' expected, ' +
      value.inspect() + ' received');
  }
}
