// Lexical analyzer. Converts the code into tokens.

// Lexical syntax complying to R6RS specification. (Actual interpreter may not
// fully implement R6RS.)

export const COMMENT = Symbol('comment');
export const COMMENT_IGNORE = Symbol('commentIgnore');
export const COMMENT_BLOCK_START = Symbol('commentBlockStart');
export const COMMENT_BLOCK_END = Symbol('commentBlockEnd');
export const IDENTIFIER = Symbol('identifier');
export const NUMBER = Symbol('number');
export const BOOLEAN = Symbol('boolean');
export const CHARACTER = Symbol('character');
export const STRING = Symbol('string');
export const BYTE_VECTOR_START = Symbol('byteVectorStart');
export const VECTOR_START = Symbol('vectorStart');
export const LIST_START = Symbol('listStart');
export const LIST_END = Symbol('listEnd');
export const LIST_CON = Symbol('listCon');
export const ABBERVIATION = Symbol('abberviation');

// Abberiviations.
export const QUOTE = Symbol('quote');
export const QUASIQUOTE = Symbol('quasiquote');
export const UNQUOTE = Symbol('unquote');
export const UNQUOTE_SPLICING = Symbol('unquote-splicing');
export const SYNTAX = Symbol('syntax');
export const QUASISYNTAX = Symbol('quasisyntax');
export const UNSYNTAX = Symbol('unsyntax');
export const UNSYNTAX_SPLICING = Symbol('unsyntax-splicing');

// Used for list brackets.
export const ROUND_BRACKET = Symbol('roundBracket');
export const SQUARE_BRACKET = Symbol('squareBracket');

const DEFAULT_STATE = 0;
const COMMENT_BLOCK_STATE = 1;

const CHAR_MAPPING = {
  nul: '\0',
  alarm: '\x07',
  backspace: '\b',
  tab: '\t',
  linefeed: '\n',
  vtab: '\v',
  page: '\x0c',
  return: '\r',
  esc: '\b',
  space: ' ',
  delete: '\xff' // Is this right?
};

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
    [/([^\s#()[\]'`0-9;"'.]|\\x[0-9a-fA-F]+)([^\s#()[\]'`;"'])*/g, (_, v) => ({
      type: IDENTIFIER,
      value: v[0]
    })],
    [new RegExp('#\\\\(x([0-9a-fA-F]+)|nul|alarm|backspace|tab|linefeed' +
      '|newline|vtab|page|return|esc|space|delete|.)', 'g'
    ), (_, v) => ({
      type: CHARACTER,
      value: CHAR_MAPPING[v[1]] || v[1].replace(/x([0-9a-fA-F]+)/g,
        (match, p1) => String.fromCharCode(parseInt(p1, 16)))
    })],
    [/"(([^\\"]|\\[abtnvfr"\\]|\\\s*\n\s*|\\x([0-9a-fA-F]+))+)"/g, (_, v) => ({
      type: STRING,
      value: v[1].replace(/\\n/g, '\n')
        .replace(/\\\\/g, '\\')
        .replace(/\\r/g, '\r')
        .replace(/\\b/g, '\b')
        .replace(/\\t/g, '\t')
        .replace(/\\v/g, '\v')
        .replace(/\\"/g, '\"')
        .replace(/\\f/g, '\f')
        // Assume \a (alarm) is \x07
        .replace(/\\a/g, '\x07')
        .replace(/\\\s*\n\s*/g, '')
        .replace(/\\x([0-9a-fA-F]+)/g,
          (match, p1) => String.fromCharCode(parseInt(p1, 16)))
    })],
    // [/((#[bBoOdDxX])?(#[iIeE])?|(#[iIeE])?(#[bBoOdDxX])?)?(\+|-)?/],
    // Support only decimals for now...
    [/(\+|-)?[0-9]+(\.[0-9]+)?(e-?[0-9]+)?/g, (_, v) => ({
      type: NUMBER,
      value: parseFloat(v[0])
    })],
    [/#(t|T|f|F)/g, (_, v) => ({
      type: BOOLEAN,
      value: v[0] === '#t' || v[0] === '#T'
    })],
    [/#vu8\(/g, () => ({ type: BYTE_VECTOR_START, value: ROUND_BRACKET })],
    [/#\(/g, () => ({ type: VECTOR_START, value: ROUND_BRACKET })],
    [/\(/g, () => ({ type: LIST_START, value: ROUND_BRACKET })],
    [/\[/g, () => ({ type: LIST_START, value: SQUARE_BRACKET })],
    [/\)/g, () => ({ type: LIST_END, value: ROUND_BRACKET })],
    [/\]/g, () => ({ type: LIST_END, value: SQUARE_BRACKET })],
    [/'/g, () => ({ type: ABBERVIATION, value: QUOTE })],
    [/`/g, () => ({ type: ABBERVIATION, value: QUASIQUOTE })],
    [/\.(?=(^|[()\[\]";#\s]))/g, () => ({ type: LIST_CON })],
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
  return output;
}
