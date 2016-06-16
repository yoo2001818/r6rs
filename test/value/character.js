import CharacterValue from '../../src/value/character';
import expect from 'expect';

describe('CharacterValue', () => {
  describe('#inspect', () => {
    it('should return correct result', () => {
      expect(new CharacterValue('t').inspect()).toBe('#\\t');
      expect(new CharacterValue('f').inspect()).toBe('#\\f');
      expect(new CharacterValue('빰').inspect()).toBe('#\\빰');
    });
    it('should escape newline, space, etc', () => {
      expect(new CharacterValue(' ').inspect()).toBe('#\\space');
      expect(new CharacterValue('\r').inspect()).toBe('#\\return');
      expect(new CharacterValue('\n').inspect()).toBe('#\\newline');
    });
  });
});
