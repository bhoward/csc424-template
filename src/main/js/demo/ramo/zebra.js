/*
The Zebra Puzzle
Here is a classic AI puzzle (snarfed from Norvig's PAIP):

There are five houses in a line, each with an owner, a pet, a cigarette, a drink, and a color.
The Englishman lives in the red house.
The Spaniard owns the dog.
Coffee is drunk in the green house.
The Ukrainian drinks tea.
The green house is immediately to the right of the ivory house.
The Winston smoker owns snails.
Kools are smoked in the yellow house.
Milk is drunk in the middle house.
The Norwegian lives in the first house on the left.
The man who smokes Chesterfields lives next to the man with the fox.
Kools are smoked in the house next to the house with the horse.
The Lucky Strike smoker drinks orange juice.
The Japanese smokes Parliaments.
The Norwegian lives next to the blue house.
The questions are: who drinks water and who owns the zebra?
*/

const { run, Rel, conde, exist, eq, firsto, resto, conso } = require('ramo');

// membero(x, xs) <= x is a member of the list xs.
const membero = Rel((x, xs) => conde(
  firsto(xs, x),
  exist(d => [
    resto(xs, d),
    membero(x, d)
  ])
));

// righto(r, l, xs) <= r is directly to the right of l in xs.
const righto = Rel((r, l, xs) => exist(d => [
  resto(xs, d),
  conde(
    [firsto(xs, l), firsto(d, r)],
    righto(r, l, d)
  )
]));

// nexto(x, y, xs) <= x is next to y in xs.
const nexto = Rel((x, y, xs) => conde(
  righto(x, y, xs),
  righto(y, x, xs)
));

// house scheme: [owner, pet, cigarette, drink, color]
const puzzleo = Rel((waterDrinker, zebraOwner, _) => exist(houses => [
  eq(houses, [_, _, _, _, _]),                                       // 1
  membero(['Englishman', _, _, _, 'red'], houses),                   // 2
  membero(['Spaniard', 'dog', _, _, _], houses),                     // 3
  membero([_, _, _, 'coffee', 'green'], houses),                     // 4
  membero(['Ukrainian', _, _, 'tea', _], houses),                    // 5
  righto([_, _, _, _, 'green'], [_, _, _, _, 'ivory'], houses),      // 6
  membero([_, 'snails', 'Winston', _, _], houses),                   // 7
  membero([_, _, 'Kools', _, 'yellow'], houses),                     // 8
  eq(houses, [_, _, [_, _, _, 'milk', _], _, _]),                    // 9
  eq(houses, [['Norwegian', _, _, _, _], _, _, _, _]),               // 10
  nexto([_, _, 'Chesterfields', _, _], [_, 'fox', _, _, _], houses), // 11
  nexto([_, _, 'Kools', _, _], [_, 'horse', _, _, _], houses),       // 12
  membero([_, _, 'Lucky Strike', 'Orange Juice', _], houses),        // 13
  membero(['Japanese', _, 'Parliaments', _, _], houses),             // 14
  nexto(['Norwegian', _, _, _, _], [_, _, _, _, 'blue'], houses),    // 15
  membero([waterDrinker, _, _, 'water', _], houses),                 // Q1
  membero([zebraOwner, 'zebra', _, _, _], houses)                    // Q2
]));

const answer = run(1)((q, _) => exist((waterDrinker, zebraOwner) => [
  eq(q, { waterDrinker, zebraOwner }),
  puzzleo(waterDrinker, zebraOwner, _)
]));
// => [{ waterDrinker: 'Norwegian', zebraOwner: 'Japanese' }]
