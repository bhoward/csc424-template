import { tokens } from './tokensCPS.mjs';
import { calc } from './calcCPS.mjs';

let source = '(1 + 2 * 3) * (45 / 6 - 3 / 2) - answer * x';

function loop(gen) {
    gen.next(t => {
        if (t.done) {
            console.log('Done');
            process.exit();
        } else {
            console.log(t.value);
            loop(gen);
        }
    });
}

// This version is a hand-converted continuation-passing-style version.
// Note that, in the absence of tail-call optimization, this will overlow
// the stack. Future work will be to implement trampolining to avoid this.

// Do one or the other of the following:

// Demo of just the token generator
// tokens(source, loop,
//     (err) => { console.error(err); process.exit(1); }
// );

// Demo of recursive descent calculator built on top of tokenizer
tokens(source, input => {
    calc(input,
        result => {
            console.log(result);
            process.exit();
        },
        err => {
            console.error(err);
            process.exit(1);
        }
    )
});
