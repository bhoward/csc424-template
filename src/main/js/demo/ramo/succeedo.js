const { run, succeedo, failo } = require('ramo');

const results1 = run()(q => [
  eq(q, 'satisfied'),
  succeedo
]);
// => ['satisfied']

const results2 = run()(q => [
  eq(q, 'unsatisfied'),
  failo
]);
// => []
