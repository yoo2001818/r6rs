import ProcedureValue from '../../src/value/procedure';
import PairValue from '../../src/value/pair';
import expect from 'expect';

describe('ProcedureValue', () => {
  describe('#inspect', () => {
    it('should return correct result', () => {
      expect(new ProcedureValue('nom', PairValue.fromArray([
        'NOM', 'NOM', 'nom'
      ])).inspect()).toBe('#<procedure nom (NOM NOM nom)>');
      expect(new ProcedureValue('color', new PairValue('red', 'etc')).inspect())
        .toBe('#<procedure color (red . etc)>');
      expect(new ProcedureValue('native', 3).inspect())
        .toBe('#<procedure native (_ _ _)>');
    });
  });
});
