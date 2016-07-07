import Machine from '../src/machine';
import expect from 'expect';

describe('Machine', () => {
  it('should preserve library cache if provided', () => {
    let cache = {};

    let machine = new Machine(true, cache);
    let machine2 = new Machine(false, cache);
    expect(machine.libraryParameters).toBe(machine2.libraryParameters);
    expect(machine.expanderLibrary).toBe(machine2.expanderLibrary);
  });
});
