const { run, eq, conde } = require('ramo');

const results1 = run()(q => conde(
  eq(q, 'first'), // q must unify with 'first'
  eq(q, 'second') // OR q must unify with 'second'
));
// => ['first', 'second']

const results2 = run()(q => [
  conde(
    eq(q, 41),
    eq(q, 42),
    eq(q, 43)
  ),
  eq({ p: q }, { p: 42 })
]);
// => [42]
