// Use Unicode general categories to test single-character strings
function isLetter(c) {
    return /^\p{L}$/u.test(c);
}

function isDigit(c) {
    return /^\p{N}$/u.test(c);
}

function isSpace(c) {
    return /^\p{Z}$/u.test(c);
}

const INIT = 0;
const ID = 1;
const NUM = 2;

const SINGLE = {
    '+': 'PLUS',
    '-': 'MINUS',
    '*': 'TIMES',
    '/': 'DIVIDE',
    '(': 'LPAREN',
    ')': 'RPAREN',
};

// State machine generator of tokens
export function* tokens(source) {
    let state = INIT;
    let first = 0;

    for (let i = 0; i <= source.length; i++) {
        let ch = (i < source.length) ? source[i] : ' ';
        switch (state) {
            case INIT:
                if (SINGLE[ch]) {
                    yield {
                        token: SINGLE[ch],
                    };
                } else if (isSpace(ch)) {
                    // do nothing
                } else if (isLetter(ch)) {
                    state = ID;
                    first = i;
                } else if (isDigit(ch)) {
                    state = NUM;
                    first = i;
                } else {
                    throw `Unexpected character: ${ch}`;
                }
                break;

            case ID:
                if (isLetter(ch) || isDigit(ch)) {
                    // do nothing
                } else {
                    yield {
                        token: 'ID',
                        lexeme: source.slice(first, i),
                    };
                    i--;
                    state = INIT;
                }
                break;

            case NUM:
                if (isDigit(ch)) {
                    // do nothing
                } else {
                    yield {
                        token: 'NUM',
                        lexeme: source.slice(first, i),
                    };
                    i--;
                    state = INIT;
                }
                break;
        }
    }
}