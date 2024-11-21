import { tokens } from './tokens.mjs';
import { calc } from './calc.mjs';

let source = '(1 + 2 * 3) * (45 / 6 - 3 / 2) - answer * x';
for (const token of tokens(source)) {
    console.log(token);
}

console.log(calc(tokens(source)));
