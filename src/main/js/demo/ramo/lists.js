const { run, conso, firsto, resto, nilo } = require('ramo');

const results1 = run()(q => conso(q, [2,3], [1,2,3]));
// => [1]

const results2 = run()(q => conso(1, q, [1,2,3]));
// => [[2,3]]

const results3 = run()(q => conso(1, [2,3], q));
// => [[1,2,3]]

const results4 = run()(q => firsto(['a', 'b', 'c'], q));
// => ['a']

const results5 = run()(q => resto(['a', 'b', 'c'], q));
// => [['b', 'c']]

const results6 = run()(q => nilo(q));
// => [[]]
