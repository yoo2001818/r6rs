export const BOOLEAN = Symbol('boolean');
export const NUMBER = Symbol('number');
export const CHARACTER = Symbol('character');
export const STRING = Symbol('string');
export const SYMBOL = Symbol('symbol');
export const PAIR = Symbol('pair');
export const VECTOR = Symbol('vector');
export const PROCEDURE = Symbol('procedure');

export { default as BooleanValue } from './boolean';
export { default as CharacterValue } from './character';
export { default as LambdaValue } from './lambda';
export { default as NativeProcedureValue } from './nativeProcedure';
export { default as NativeSyntaxValue } from './nativeSyntax';
export { default as NumberValue } from './number';
export { default as PairValue } from './pair';
export { default as ProcedureValue } from './procedure';
export { default as StringValue } from './string';
export { default as SymbolValue } from './symbol';
