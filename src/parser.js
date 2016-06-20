import BooleanValue from './value/boolean';
import CharacterValue from './value/character';
import RealValue from './value/number';
import PairValue from './value/pair';
import StringValue from './value/string';
import SymbolValue from './value/symbol';

import * as TOKENS from './tokenizer';

const LIST = Symbol('list');

const ROOT = Symbol('root');

function createStackEntry(type, bracketType) {
  return {
    head: null,
    tail: null,
    type, // Only list is supported for now..
    bracketType,
    special: null, // COMMENT_IGNORE, QUOTE, QUASIQUOTE, etc
    con: 0 // Is . specified? 0 - No, 1 - Yes, it is, 2 - Yes, it did
  };
}

// Pushes the data into the stack entry. Note that this doesn't push the data
// into the stack..
function pushData(entry, data) {
  let pair = new PairValue(data, null);
  // TODO This should process cons too
  if (entry.tail == null) {
    // Welp, if tail is missing, head is missing too (of course)
    entry.head = entry.tail = pair;
  } else {
    // If not, just attach the value.
    entry.tail.cdr = pair;
    entry.tail = pair;
  }
}

// Accepts tokens produced by tokenizer, performs syntax analysis and
// returns an AST (S-expression) processable by the interpreter.
export default function parse(tokens) {
  let stack = [createStackEntry(ROOT)];
  for (let i = 0; i < tokens.length; ++i) {
    let stackEntry = stack[stack.length - 1];
    let token = tokens[i];
    console.log(stackEntry, token);
    switch (token.type) {
    case TOKENS.LIST_START:
      stack.push(createStackEntry(LIST, token.value));
      break;
    case TOKENS.LIST_END:
      // Finalize the current stack..
      // Pop the stack, and refer parent stack.
      stack.pop();
      console.log(stackEntry.head);
      pushData(stack[stack.length - 1], stackEntry.head);
      break;
    case TOKENS.IDENTIFIER:
      pushData(stackEntry, new SymbolValue(token.value));
      break;
    case TOKENS.STRING:
      pushData(stackEntry, new StringValue(token.value));
      break;
    case TOKENS.NUMBER:
      pushData(stackEntry, new RealValue(token.value));
      break;
    case TOKENS.BOOLEAN:
      pushData(stackEntry, new BooleanValue(token.value));
      break;
    case TOKENS.CHARACTER:
      pushData(stackEntry, new CharacterValue(token.value));
      break;
    }
  }
  return stack[0].head;
}
