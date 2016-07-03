import { SYMBOL, PAIR } from '../value';
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
    let patternLen = pattern ? pattern.length(false) : 0;
    let codeLen = code ? code.length(false) : 0;
    let patternCur = pattern;
    let patternNext = pattern.cdr;
    let patternEllipsis = patternNext && patternNext.type === PAIR &&
        patternNext.car.type === SYMBOL && patternNext.car.value === '...';
    let codeNode = code;
    while (patternCur != null && patternCur.type === PAIR) {
      if (codeNode == null && (patternEllipsis || parentEllipsis)) {
        if (patternCur.car.type === SYMBOL &&
          scope[patternCur.car.value] == null
        ) {
          // Create empty value to prevent template error
          scope[patternCur.car.value] = {
            type: LIST_WRAP
          };
        } else if (patternCur.car.type === PAIR) {
          // Create empty value (recursion)
          this.checkPattern(patternCur.car,
            null, scope,
            patternEllipsis);
        }
        patternCur = patternNext;
        patternNext = patternNext && patternNext.cdr;
        patternEllipsis = patternNext && patternNext.type === PAIR &&
          patternNext.car.type === SYMBOL && patternNext.car.value === '...';
        patternLen --;
        continue;
      }
      if (patternCur.car == null) {
        // Do absolutely nothing
        if (codeNode != null && codeNode.car != null) {
          // Null check fail
          return false;
        }
      } else if (patternCur.car.type === SYMBOL) {
        if (patternCur.car.value === '...') {
          // This should just be skipped, without triggering code change
          patternCur = patternNext;
          patternNext = patternNext && patternNext.cdr;
          patternEllipsis = patternNext && patternNext.type === PAIR &&
            patternNext.car.type === SYMBOL && patternNext.car.value === '...';
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
        if (codeNode.car == null) {
          let result = this.checkPattern(patternCur.car,
            new PairValue(), scope,
            patternEllipsis);
          if (!result) return false;
        } else {
          if (codeNode.car.type !== PAIR) return false;
          let result = this.checkPattern(patternCur.car,
            codeNode.car, scope,
            patternEllipsis);
          if (!result) return false;
        }
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
      patternEllipsis = patternNext && patternNext.type === PAIR &&
        patternNext.car.type === SYMBOL && patternNext.car.value === '...';
      patternLen --;
    }
    // Check CDR value too
    // TODO We should merge this with top node
    if (patternCur != null) {
      // Underflow
      if (patternCur.type === SYMBOL) {
        if (codeNode == null) {
          // If codeNode is null - it failed (underflow).
          if (codeNode == null) return false;
        } else if (patternCur.value === '_') {
          // This always matches
        } else if (this.literals.indexOf(patternCur.value) !== -1) {
          // Literal identifier; check if code also matches.
          if (codeNode.type !== SYMBOL ||
            patternCur.value !== codeNode.value
          ) return false;
        } else {
          // Pattern variable...
          // TODO check confliction
          if (!parentEllipsis) {
            // Flat pattern. We can just copy it to the scope
            scope[patternCur.value] = codeNode;
          } else {
            // Ellipsis pattern. We have to store it on the list.
            let varData = scope[patternCur.value];
            let pair = new PairValue(codeNode, null);
            if (varData) {
              varData.tail.cdr = pair;
              varData.tail = pair;
            } else {
              scope[patternCur.value] = {
                type: LIST_WRAP,
                head: pair,
                tail: pair
              };
            }
          }
        }
      } else if (patternCur.type === PAIR) {
        if (codeNode == null) {
          let result = this.checkPattern(patternCur,
            new PairValue(), scope,
            patternEllipsis);
          if (!result) return false;
        } else {
          if (codeNode.type !== PAIR) return false;
          let result = this.checkPattern(patternCur,
            codeNode, scope,
            patternEllipsis);
          if (!result) return false;
        }
      } else {
        // Nope
        throw new Error('Pattern datum is not supported yet. Please use ' +
          'if (or when) in the template for now!');
      }
    } else if (codeNode != null) {
      // It failed (Overflow)
      return false;
    }
    return true;
  }
  runTemplate(template, scope, listLoc = {}) {
    if (template.type !== PAIR) {
      // This is a special case, where only symbol or constant value can exist.
      if (template.type === SYMBOL) {
        let scopeValue = scope[template.value];
        if (scopeValue) {
          if (scopeValue.type === LIST_WRAP) {
            // I think this won't get a list though.
            if (listLoc[template.value] == null) {
              listLoc[template.value] = scopeValue.head;
            }
            let listValue = listLoc[template.value];
            if (listValue === false) {
              // Underflow.
              return false;
            }
            listLoc[template.value] = listValue.cdr || false;
            return listValue.car;
          } else {
            return scopeValue;
          }
        } else {
          return template;
        }
      } else {
        // Just return template if this is a constant value.
        return template;
      }
    }
    // This doesn't support (... ...) producing single ellipsis!
    let outputHead, outputTail;
    let cur = template;
    let next = template.cdr;
    let ellipsis = next && next.type === PAIR &&
        next.car.type === SYMBOL && next.car.value === '...';
    let cdrNode = false;
    function pushOutput(data) {
      if (cdrNode) {
        if (outputHead) {
          outputTail.cdr = data;
        } else {
          // This isn't really possible though.
          outputHead = outputTail = new PairValue(null, data);
        }
        return;
      }
      let pair = new PairValue(data);
      if (outputHead) {
        outputTail.cdr = pair;
        outputTail = pair;
      } else {
        outputHead = outputTail = pair;
      }
    }
    while (cur != null) {
      let current;
      if (cur.type === PAIR) {
        current = cur.car;
      } else {
        current = cur;
        cdrNode = true;
      }
      if (current == null) {
        // Null value
        if (ellipsis) throw new Error('Unexpected ellipsis');
      } else if (current.type === PAIR) {
        if (ellipsis) {
          // Run template until false comes out. Weird, huh?
          let listObj = Object.assign({}, listLoc);
          let output = this.runTemplate(current, scope, listObj);
          while (output !== false) {
            pushOutput(output);
            output = this.runTemplate(current, scope, listObj);
          }
        } else {
          let output = this.runTemplate(current, scope, listLoc);
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
              let node = scopeValue.head;
              while (node != null) {
                pushOutput(node.car);
                node = node.cdr;
              }
            } else {
              // What??
              throw new Error(current.value + ' has unexpected ellipsis');
            }
          } else {
            if (scopeValue) {
              if (scopeValue.type === LIST_WRAP) {
                if (listLoc[current.value] == null) {
                  listLoc[current.value] = scopeValue.head;
                }
                let listValue = listLoc[current.value];
                if (listValue === false || listValue == null) {
                  // Underflow.
                  return false;
                }
                pushOutput(listValue.car);
                listLoc[current.value] = listValue.cdr || false;
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
      ellipsis = next && next.type === PAIR &&
        next.car.type === SYMBOL && next.car.value === '...';
    }
    return outputHead || new PairValue();
  }
  exec(code) {
    let node = this.rules;
    while (node != null) {
      let scope = {};
      if (this.checkPattern(node.car.car, code, scope)) {
        // Transform the code using template
        let result = this.runTemplate(node.car.cdr.car, scope);
        if (result === false) throw new Error('Data underflow');
        return result;
      }
      node = node.cdr;
    }
    return code;
  }
}
