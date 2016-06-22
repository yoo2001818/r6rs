import { SYMBOL, PAIR } from '../value/value';
import PairValue from '../value/pair';

export default class SyntaxRules {
  constructor(list) {
    this.literals = [];
    list.cdr.car.forEach(literal => {
      if (literal.type !== SYMBOL) throw new Error('Literal must be a symbol');
      this.literals.push(literal);
    });
    this.rules = list.cdr.cdr;
  }
  checkPattern(pattern, code, scope, parentEllipsis = false) {
    let patternLen = pattern.length(false);
    let codeLen = code.length(false);
    let patternCur = pattern;
    let patternNext = pattern.cdr;
    let patternEllipsis = patternNext && patternNext.car.type === SYMBOL &&
      patternNext.car.value === '...';
    let codeNode = code;
    while (patternCur != null && patternCur.type === PAIR) {
      console.log(patternCur.car, patternNext && patternNext.car,
        codeNode && codeNode.car, patternLen, codeLen);
      if (codeNode == null && patternEllipsis) break;
      if (patternCur.car.type === SYMBOL) {
        if (patternCur.car.value === '...') {
          // This should just be skipped, without triggering code change
          patternCur = patternNext;
          patternNext = patternNext && patternNext.cdr;
          patternEllipsis = patternNext && patternNext.car.type === SYMBOL &&
            patternNext.car.value === '...';
          patternLen --;
          continue;
        } else if (codeNode == null) {
          // If codeNode is null - it failed (underflow).
          if (codeNode == null) return false;
        } else if (patternCur.car.value === '_') {
          // This always matches
        } else if (this.literals.indexOf(patternCur.car.value) !== -1) {
          // Literal identifier; check if code also matches.
          if (codeNode.car.type !== SYMBOL ||
            patternCur.car.value !== codeNode.car.value
          ) return false;
        } else {
          // Pattern variable...
          // TODO check confliction
          if (!parentEllipsis && !patternEllipsis) {
            // Flat pattern. We can just copy it to the scope
            scope[patternCur.car.value] = codeNode.car;
          } else {
            // Ellipsis pattern. We have to store it on the list.
            let varData = scope[patternCur.car.value];
            let pair = new PairValue(codeNode.car, null);
            if (varData) {
              varData.tail.cdr = pair;
              varData.tail = pair;
            } else {
              scope[patternCur.car.value] = {
                head: pair,
                tail: pair
              };
            }
          }
        }
      } else if (patternCur.car.type === PAIR) {
        this.checkPattern(patternCur.car, codeNode.car, scope, patternEllipsis);
      } else {
        // Nope
        throw new Error('Not implemented yet');
      }
      codeNode = codeNode.cdr;
      codeLen --;
      if (patternEllipsis) {
        if (patternLen - 2 < codeLen) {
          // Ellipsis repeat.
          continue;
        }
      }
      patternCur = patternNext;
      patternNext = patternNext && patternNext.cdr;
      patternEllipsis = patternNext && patternNext.car.type === SYMBOL &&
        patternNext.car.value === '...';
      patternLen --;
    }
    if (codeNode != null && codeNode.type === PAIR) {
      // It failed (Overflow)
      return false;
    }
    // Check CDR value too
    return true;
  }
  exec(code) {
    let node = this.rules;
    while (node != null) {
      let scope = {};
      if (this.checkPattern(node.car.car, code, scope)) {
        // Transform the code using template
        console.log(scope);
      }
      node = node.cdr;
    }
    return code;
  }
}
