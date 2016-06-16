import StringValue from '../../src/value/string';
import expect from 'expect';

describe('StringValue', () => {
  describe('#inspect', () => {
    it('should return correct result', () => {
      expect(new StringValue('Hello world!').inspect()).toBe('"Hello world!"');
      expect(new StringValue('').inspect()).toBe('""');
      expect(new StringValue(' ').inspect()).toBe('" "');
    });
    it('should escape newline, etc', () => {
      expect(new StringValue('Nope\rNope').inspect()).toBe('"Nope\\rNope"');
      expect(new StringValue('Nope\nNope').inspect()).toBe('"Nope\\nNope"');
      expect(new StringValue('Nope\tNope').inspect()).toBe('"Nope\\tNope"');
      expect(new StringValue('Nope"Nope').inspect()).toBe('"Nope\\"Nope"');
      expect(new StringValue('Nope\\Nope').inspect()).toBe('"Nope\\\\Nope"');
    });
  });
});
