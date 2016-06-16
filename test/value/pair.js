import BooleanValue from '../../src/value/boolean';
import PairValue from '../../src/value/pair';
import expect from 'expect';

function checkLoopAll(name, times, list, returnVal) {
  let i = 0;
  list[name]((entry, index, arr) => {
    expect(entry).toBe(String.fromCharCode('a'.charCodeAt() + i));
    expect(index).toBe(i);
    expect(arr).toBe(list);
    i ++;
    return returnVal;
  });
  expect(i).toBe(times);
}

describe('PairValue', () => {
  let testPair;
  let testPair2;
  beforeEach('init test list', () => {
    testPair = new PairValue('a',
      new PairValue('b',
        new PairValue('c',
          new PairValue('d', null)
        )
      )
    );
    testPair2 = new PairValue('a',
      new PairValue('b', 'c')
    );
  });
  describe('#length', () => {
    it('should return correct result', () => {
      expect(testPair.length()).toBe(4);
      expect(testPair2.length()).toBe(3);
    });
  });
  describe('#forEach', () => {
    it('should call function with correct values', () => {
      checkLoopAll('forEach', 4, testPair);
      checkLoopAll('forEach', 3, testPair2);
    });
    it('should call function with thisArg', () => {
      let thisObj = 'Hello there!';
      testPair.forEach(function () {
        expect(this).toBe(thisObj);
      }, thisObj);
    });
  });
  describe('#every', () => {
    it('should call function with correct values', () => {
      checkLoopAll('every', 4, testPair, true);
      checkLoopAll('every', 3, testPair2, true);
    });
    it('should return correct result', () => {
      expect(testPair.every(() => true)).toBe(true);
      expect(testPair.every(() => false)).toBe(false);
      expect(testPair.every((_, i) => i < 3)).toBe(false);
    });
    it('should call function with thisArg', () => {
      let thisObj = 'Hello there!';
      testPair.every(function () {
        expect(this).toBe(thisObj);
      }, thisObj);
    });
  });
  describe('#some', () => {
    it('should call function with correct values', () => {
      checkLoopAll('some', 4, testPair, false);
      checkLoopAll('some', 3, testPair2, false);
    });
    it('should return correct result', () => {
      expect(testPair.some(() => true)).toBe(true);
      expect(testPair.some(() => false)).toBe(false);
      expect(testPair.some((_, i) => i >= 3)).toBe(true);
      expect(testPair.some((_, i) => i === 0)).toBe(true);
    });
    it('should call function with thisArg', () => {
      let thisObj = 'Hello there!';
      testPair.some(function () {
        expect(this).toBe(thisObj);
      }, thisObj);
    });
  });
  describe('#toArray', () => {
    it('should return correct array', () => {
      expect(testPair.toArray()).toEqual(['a', 'b', 'c', 'd']);
      expect(testPair2.toArray()).toEqual(['a', 'b', 'c']);
    });
  });
  describe('#fromArray', () => {
    it('should return correct list', () => {
      let result = PairValue.fromArray(['b', 'o', 'o', 'p']);
      expect(result.toArray()).toEqual(['b', 'o', 'o', 'p']);
    });
  });
  describe('#filter', () => {
    it('should call function with correct values', () => {
      checkLoopAll('filter', 4, testPair, true);
      checkLoopAll('filter', 3, testPair2, true);
    });
    it('should return correct list', () => {
      expect(testPair.filter(() => true).toArray()).toEqual(testPair.toArray());
      expect(testPair.filter(() => false).toArray()).toEqual([]);
      expect(testPair.filter((_, i) => i >= 3).toArray()).toEqual(['d']);
      expect(testPair.filter((_, i) => i === 0).toArray()).toEqual(['a']);
      expect(testPair2.filter(() => true).inspect()).toEqual('(a b . c)');
      expect(testPair2.filter(e => e !== 'b').inspect()).toEqual('(a . c)');
    });
    it('should call function with thisArg', () => {
      let thisObj = 'Hello there!';
      testPair.filter(function () {
        expect(this).toBe(thisObj);
      }, thisObj);
    });
  });
  describe('#map', () => {
    it('should call function with correct values', () => {
      checkLoopAll('map', 4, testPair, true);
      checkLoopAll('map', 3, testPair2, true);
    });
    it('should return correct list', () => {
      expect(testPair.map((_, i) => i).toArray()).toEqual([0, 1, 2, 3]);
      expect(testPair2.map((_, i) => i).inspect()).toEqual('(0 1 . 2)');
    });
    it('should call function with thisArg', () => {
      let thisObj = 'Hello there!';
      testPair.filter(function () {
        expect(this).toBe(thisObj);
      }, thisObj);
    });
  });
  describe('#concat', () => {
    it('should combine two list', () => {
      expect(testPair.concat(testPair).inspect()).toEqual('(a b c d a b c d)');
      expect(testPair2.concat(testPair2).inspect()).toEqual('(a b c a b . c)');
    });
  });
  describe('#inspect', () => {
    it('should return correct string', () => {
      expect(testPair.inspect()).toBe('(a b c d)');
      expect(testPair2.inspect()).toBe('(a b . c)');
    });
    it('should call inspect function if possible', () => {
      let result = PairValue.fromArray([
        new BooleanValue(true),
        PairValue.fromArray([new BooleanValue(false), 'yes'])
      ]);
      expect(result.inspect()).toBe('(#t (#f yes))');
    });
  });
});
