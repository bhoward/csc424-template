const { run, eq, exist } = require('ramo');

run()(q => exist((x, y) => [
  eq(x, 'one'),
  eq(y, false),
  eq(q, [x, y])
]));
// => [['one', false]]
