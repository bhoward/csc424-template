const { run, Rel, eq, conde, exist, firsto, resto } = require('ramo');

// membero(x, xs) <= x is a member of the list xs.
const membero = Rel((x, xs) => conde(
  firsto(xs, x),
  exist(rest => [
    resto(xs, rest),
    membero(x, rest)
  ])
));

const results1 = run()(q => membero(1, [1,2,3]));
// => ['_0']

const results2 = run()(q => membero(q, [1,2,3]));
// => [1,2,3]
