const { run, Rel, conde, exist, eq, conso, nilo } = require('ramo');

const appendo = Rel((xs, ys, zs) => conde(
  [nilo(xs), eq(ys, zs)],
  exist((a, d, res) => [
    conso(a, d, xs),
    conso(a, res, zs),
    appendo(d, ys, res)
  ])
));

// It can be used to append two lists:
const results1 = run()(zs => appendo([1,2,3], [4,5], zs));
// => [[1,2,3,4,5]]

// Or to generate one given the other two:
const results2 = run()(xs => appendo(xs, [4,5], [1,2,3,4,5]));
// => [[1,2,3]]

// Or to generate all possible pairs that append to a third:
const results3 = run()(q => exist((xs, ys) => [
  eq(q, [xs, ys]),
  appendo(xs, ys, [1,2,3])
]));
// => [[[], [1,2,3]],
//     [[1], [2,3]],
//     [[1,2], [3]],
//     [[1,2,3], []]]
