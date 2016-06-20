// Lexical analyzer. Converts the code into tokens.

// Lexical syntax complying to R6RS specification. (Actual interpreter may not
// fully implement R6RS.)

const COMMENT = Symbol('comment');
const COMMENT_IGNORE = Symbol('commentIgnore');
const COMMENT_BLOCK_START = Symbol('commentBlockStart');
const COMMENT_BLOCK_END = Symbol('commentBlockEnd');
const IDENTIFIER = Symbol('identifier');
const NUMBER = Symbol('number');
const BOOLEAN = Symbol('boolean');
const CHARACTER = Symbol('character');
const STRING = Symbol('string');
const BYTE_VECTOR_START = Symbol('byteVectorStart');
const VECTOR_START = Symbol('vectorStart');
const LIST_START = Symbol('listStart');
const LIST_END = Symbol('listEnd');
const LIST_CON = Symbol('listCon');
const QUOTE = Symbol('quote');
const QUASIQUOTE = Symbol('quasiquote');

// Used for list brackets.
const ROUND_BRACKET = Symbol('roundBracket');
const SQUARE_BRACKET = Symbol('squareBracket');

const DEFAULT_STATE = 0;
const COMMENT_BLOCK_STATE = 1;

const SYNTAX_TABLE = [
  // default state
  [
    [/;(.+)$|(#!r(5|6|7)rs)/gm, () => ({ type: COMMENT })],
    [/#;/g, () => ({ type: COMMENT_IGNORE })],
    [/#\|/g, machine => {
      machine.state = COMMENT_BLOCK_STATE;
      machine.depth = 0;
      return { type: COMMENT_BLOCK_START };
    }],
    // [/[a-zA-Z!$%&*/:<=>?\^_~+\-]/]
    // peculiar identifier is not implemented yet
    [/([^\s#()[\]'`0-9;]|\\x[0-9a-fA-F]+)([^\s#()[\]'`;])*/g, (_, v) => ({
      type: IDENTIFIER,
      value: v[0]
    })],
    [new RegExp('#\\\\(x([0-9a-fA-F]+)|nul|alarm|backspace|tab|linefeed' +
      '|newline|vtab|page|return|esc|space|delete|.)', 'g'
    ), () => ({
      type: CHARACTER
    })],
    [/"([^\\"]|\\[abtnvfr"\\]|\\\s*\n\s*|\\x([0-9a-fA-F]+))+"/g, () => ({
      type: STRING
    })],
    // [/((#[bBoOdDxX])?(#[iIeE])?|(#[iIeE])?(#[bBoOdDxX])?)?(\+|-)?/],
    // Support only decimals for now...
    [/(\+|-)?[0-9]+(\.[0-9]+)?(e-?[0-9]+)?/g, (_, v) => ({
      type: NUMBER,
      value: v[0]
    })],
    [/#(t|T|f|F)/g, () => ({ type: BOOLEAN })],
    [/#vu8\(/g, () => ({ type: BYTE_VECTOR_START, value: ROUND_BRACKET })],
    [/#\(/g, () => ({ type: VECTOR_START, value: ROUND_BRACKET })],
    [/\(/g, () => ({ type: LIST_START, value: ROUND_BRACKET })],
    [/\[/g, () => ({ type: LIST_START, value: SQUARE_BRACKET })],
    [/\)/g, () => ({ type: LIST_END, value: ROUND_BRACKET })],
    [/\]/g, () => ({ type: LIST_END, value: SQUARE_BRACKET })],
    [/'/g, () => ({ type: QUOTE })],
    [/`/g, () => ({ type: QUASIQUOTE })],
    [/\./g, () => ({ type: LIST_CON })],
    [/\s+/g, () => undefined]
    // TODO: Implement , ,@ #' #` #, #,@
  ], [ // command block state
    [/#\|/g, machine => {
      machine.depth ++;
      return { type: COMMENT_BLOCK_START };
    }],
    [/\|#/g, machine => {
      if (machine.depth === 0) {
        machine.state = DEFAULT_STATE;
      } else {
        machine.depth --;
      }
      return { type: COMMENT_BLOCK_END };
    }],
    [/([^\|#]+|.)/g, () => undefined]
  ]
];

export default function tokenize(code) {
  let machine = {
    state: 0
  };
  let output = [];
  let index = 0;
  while (index < code.length) {
    // Cycle through all syntaxes...
    let syntaxes = SYNTAX_TABLE[machine.state];
    let results = [];
    for (let i = 0; i < syntaxes.length; ++i) {
      let syntax = syntaxes[i];
      let pattern = syntax[0];
      pattern.lastIndex = index;
      let result = pattern.exec(code);
      if (!result || result.index !== index) continue;
      let callback = syntax[1];
      results.push([result, callback]);
    }
    // results.sort((a, b) => a[0].length - b[0].length);
    if (results.length === 0) {
      throw new Error('Tokenizer failed');
    }
    let next = false;
    for (let i = 0; i < results.length; ++i) {
      let result = results[i][1](machine, results[i][0]);
      if (result !== undefined) {
        output.push(result);
        index = results[i][0][0].length + index;
        next = true;
        break;
      }
    }
    if (!next) {
      index = results[results.length - 1][0][0].length + index;
    }
  }
  console.log(output);
}
