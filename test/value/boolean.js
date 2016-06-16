import BooleanValue from '../../src/value/boolean';
import expect from 'expect';

describe('BooleanValue', () => {
  describe('#inspect', () => {
    it('should return correct result', () => {
      expect(new BooleanValue(true).inspect()).toBe('#t');
      expect(new BooleanValue(false).inspect()).toBe('#f');
    });
  });
});
