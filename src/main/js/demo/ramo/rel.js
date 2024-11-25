const { run, Rel, eq, conde } = require('ramo');

const eitherOro = Rel(q => conde(
  eq(q, 'either'),
  eq(q, 'or')
));

const results = run()(q => eitherOro(q));
// => ['either', 'or']
