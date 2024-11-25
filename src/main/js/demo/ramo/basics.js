const { run, eq } = require('ramo');

const g1 = eq(3, 3); // This is a goal that succeeds if 3 === 3
const results1 = run()(q => g1); // => ['_0'] (a single success)

const g2 = eq(3, 'three'); // This is a goal that succeeds if 3 === 'three'
const results2 = run()(q => g2); // => [] (no successes)

const results3 = run()(q => eq(q, 3)); // => [3]

const results4 = run()(q => eq(q, 'three')); // => ['three']

const results5 = run()(q => eq(q, { complex: 'val' })); // => [{ complex: 'val' }]

const results6 = run()(q => eq(q, { contains: q })); // => []

const results7 = run()(q => eq({ includes: q }, { includes: { another: 'object' } }));
// => [{ another: 'object' }]

const results8 = run()(q => [
  eq(q, 'first'), // q must unify with 'first'
  eq(42, 42)      // AND 42 must unify with 42
]);
// => ['first']

const results9 = run()(q => [
  eq(q, 'first'), // q must unify with 'first'
  eq(q, 'second') // AND q must unify with 'second'
]);
// => []
