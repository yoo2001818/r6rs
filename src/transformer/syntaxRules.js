import { SYMBOL, PAIR } from '../value/value';
import PairValue from '../value/pair';

const LIST_WRAP = Symbol('listWrap');

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
      if (codeNode == null && patternEllipsis) {
        if (patternCur.car.type === SYMBOL &&
          scope[patternCur.car.value] == null
        ) {
          // Create empty value to prevent template error
          scope[patternCur.car.value] = {
            type: LIST_WRAP
          };
        }
        break;
      }
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
                type: LIST_WRAP,
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
        throw new Error('Pattern datum is not supported yet. Please use ' +
          'if (or when) in the template for now!');
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
  runTemplate(template, scope) {
    // This doesn't support (... ...) producing single ellipsis!
    let outputHead, outputTail;
    let cur = template;
    let next = template.cdr;
    let ellipsis = next && next.car.type === SYMBOL && next.car.value === '...';
    function pushOutput(data) {
      let pair = new PairValue(data);
      if (outputHead) {
        outputTail.cdr = pair;
        outputTail = pair;
      } else {
        outputHead = outputTail = pair;
      }
    }
    while (cur != null && cur.type === PAIR) {
      console.log(cur.car, next && next.car);
      let current = cur.car;
      if (current.type === PAIR) {
        if (ellipsis) {
          // Run template until false comes out. Weird, huh?
          let output = this.runTemplate(current, scope);
          while (output !== false) {
            pushOutput(output);
            output = this.runTemplate(current, scope);
          }
        } else {
          let output = this.runTemplate(current, scope);
          if (output === false) return false;
          pushOutput(output);
        }
      } else if (current.type === SYMBOL) {
        // Ignore ...
        if (current.value !== '...') {
          let scopeValue = scope[current.value];
          if (ellipsis) {
            if (scopeValue && scopeValue.type === LIST_WRAP) {
              // Simply loop and copy all the values from the list.
              while (scopeValue.head != null) {
                pushOutput(scopeValue.head.car);
                scopeValue.head = scopeValue.head.cdr;
              }
            } else {
              // What??
              throw new Error(current.value + ' has unexpected ellipsis');
            }
          } else {
            if (scopeValue) {
              if (scopeValue.type === LIST_WRAP) {
                if (scopeValue.head == null) {
                  // Underflow.
                  return false;
                }
                pushOutput(scopeValue.head.car);
                scopeValue.head = scopeValue.head.cdr;
              } else {
                pushOutput(scopeValue);
              }
            } else {
              pushOutput(current);
            }
          }
        }
      } else {
        // Constant value
        if (ellipsis) throw new Error('Unexpected ellipsis');
        pushOutput(current);
      }
      cur = next;
      next = next && next.cdr;
      ellipsis = next && next.car.type === SYMBOL && next.car.value === '...';
    }
    return outputHead;
  }
  exec(code) {
    let node = this.rules;
    while (node != null) {
      let scope = {};
      if (this.checkPattern(node.car.car, code, scope)) {
        // Transform the code using template
        console.log(scope);
        let result = this.runTemplate(node.car.cdr.car, scope);
        if (result === false) throw new Error('Data underflow');
        return result;
      }
      node = node.cdr;
    }
    return code;
  }
}
