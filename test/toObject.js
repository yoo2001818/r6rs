import tokenize from '../src/tokenizer';
import parse from '../src/parser';

import toObject, { fromAssoc } from '../src/util/toObject';

console.log(fromAssoc(toObject(parse(tokenize(`
(a 1) (b 2) (c 3) (d (c 1))
`)))));
