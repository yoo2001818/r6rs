# r6rs
A Scheme interpreter (Currently partly R6RS compliant)

This program is still work in progress! Use it on your own risk!

# Usage
`npm install r6rs` will do the trick.

```js
import { Machine } from 'r6rs';
let machine = new Machine();
console.log(machine.evaluate('(+ 3 3)')); // NumberValue {value: 6}
```

## Manually calling with AST
```js
import { tokenize, parse, Machine } from 'r6rs';
let machine = new Machine();
console.log(machine.evaluate(expand(parse(tokenize('(+ 3 3)')), machine.expanderRoot))); // NumberValue {value: 6}
// Note that you can skip expand phase if you don't want to use macro expansion
// (Which includes let, letrec, and, ...)
```

## Using native functions
```js
import { Machine, NativeProcedureValue, PairValue } from 'r6rs';
let machine = new Machine();
machine.loadLibrary([
  new NativeProcedureValue('alert', list => {
    alert(list.car.value);
    return new PairValue();
  })
]);
console.log(machine.evaluate('(alert "Hello, world!")')); // ()
```

## Using REPL
`npm install r6rs -g` then `r6rs` will do the trick. :)

# Implementation status

## R6RS specification

### Types
- [x] Boolean
- [ ] Number (Only real numbers are supported for now)
- [x] Character
- [ ] Vector
- [x] String
- [x] Symbol
- [x] Pair (List)
- [x] Procedure
- [ ] Bytevector
- [x] Abbreviations (Not a type though)

### Number
- [ ] Complex number
- [x] Real (decimal) number
- [ ] Rational number
- [x] Integer number
- [x] NaN, infinity support

### Library
- [ ] Library syntax support

### Base library
- [x] define, define-syntax
- [x] quote, quasiquote, unquote
- [x] lambda, if, set!
- [ ] cond, case (Only cond is supported)
- [x] and, or
- [ ] `let`, `let*`, `letrec`, `letrec*`, `let-values`, `let*-values` (only `let`, `let*` can be used due to lack of hygiene)
- [x] begin
- [x] eqv?, eq?, equal?
- [x] `(type name)?`
- [x] `(type name)->(type name)` (number->string doesn't support precision yet)
- [ ] Rational number arithmetic operations (numerator, denominator, exact?, ...)
- [x] Real number arithmetic operations
- [ ] Complex number arithmetic operations (make-rectangular, ...)
- [x] Boolean operations
- [x] Pair and list operations
- [x] Symbol operations
- [x] Character operations
- [x] String operations
- [ ] Vector operations
- [x] Errors and violations
- [ ] Control features (no call/cc yet)
- [x] Iteration let
- [ ] let-syntax, letrec-syntax (not correctly implemented)
- [x] syntax-rules (Hygienic use is not supported yet)
- [ ] identifier-syntax

### Etc
- [x] Tail call optimization

## R6RS standard libraries specification
- [x] Unicode operations
- [ ] Bytevector operations
- [ ] List utilities
- [ ] Sorting
- [ ] Control structures
- [ ] Records
- [ ] Exceptions and conditions
- [ ] ~~I/O~~
- [ ] ~~File system~~
- [ ] ~~Command-line access and exit values~~
- [ ] Arithmetic
  - [ ] Fixnums
  - [ ] Flonums
  - [ ] Exact bitwise arithmetic
- [ ] syntax-case
- [ ] Hashtables
- [ ] Enumerations
- [ ] eval
- [ ] Mutable pairs
- [ ] Mutable strings

## SRFI
Nothing is supported yet
