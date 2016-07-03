// Simple utility function that converts r6rs objects to JavaScript objects
import { PAIR, BOOLEAN, NUMBER, CHARACTER, STRING, SYMBOL }
  from '../value/type';

export default function toObject(obj) {
  if (obj.type === BOOLEAN) return obj.value;
  if (obj.type === NUMBER) return obj.value;
  if (obj.type === CHARACTER) return obj.value;
  if (obj.type === STRING) return obj.value;
  if (obj.type === SYMBOL) return obj.value;
  if (obj.type === PAIR) {
    let arr = obj.toArray();
    arr = arr.map(toObject);
    return arr;
  }
  throw new Error('Unknown object type ' + obj.inspect());
}

// Converts assoc array (JS) to object
export function fromAssoc(obj) {
  let output = {};
  obj.forEach(v => {
    output[v[0]] = v[1];
  });
  return output;
}
