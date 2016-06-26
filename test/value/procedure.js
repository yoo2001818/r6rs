import ProcedureValue from '../../src/value/procedure';
import PairValue from '../../src/value/pair';
import expect from 'expect';

describe('ProcedureValue', () => {
  describe('#inspect', () => {
    it('should return correct result', () => {
      expect(new ProcedureValue('nom', null, PairValue.fromArray([
        'NOM', 'NOM', 'nom'
      ])).inspect()).toBe('#<procedure nom (NOM NOM nom)>');
      expect(new ProcedureValue('color', null, new PairValue('red', 'etc'))
        .inspect()).toBe('#<procedure color (red . etc)>');
    });
  });
});
