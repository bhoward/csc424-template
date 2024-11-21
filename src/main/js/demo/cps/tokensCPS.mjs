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
export function tokens(source, ret, thro) {
    function aux(state, first, i, yld) {
        if (i <= source.length) {
            let ch = (i < source.length) ? source[i] : ' ';
            switch (state) {
                case INIT:
                    if (SINGLE[ch]) {
                        yld({
                            token: SINGLE[ch],
                        },
                        y => aux(INIT, first, i + 1, y));
                    } else if (isSpace(ch)) {
                        aux(INIT, first, i + 1, yld);
                    } else if (isLetter(ch)) {
                        aux(ID, i, i + 1, yld);
                    } else if (isDigit(ch)) {
                        aux(NUM, i, i + 1, yld);
                    } else {
                        thro(`Unexpected character: ${ch}`);
                    }
    
                case ID:
                    if (isLetter(ch) || isDigit(ch)) {
                        aux(ID, first, i + 1, yld);
                    } else {
                        yld({
                            token: 'ID',
                            lexeme: source.slice(first, i),
                        },
                        y => aux(INIT, first, i, y));
                    }
    
                case NUM:
                    if (isDigit(ch)) {
                        aux(NUM, first, i + 1, yld);
                    } else {
                        yld({
                            token: 'NUM',
                            lexeme: source.slice(first, i),
                        },
                        y => aux(INIT, first, i, y));
                    }
            }
        } else {
            yld(undefined);
        }
    }
    
    const generator = {};
    generator.resume = k => aux(INIT, 0, 0, k);
    generator.next = ret => {
        generator.resume((value, res) => {
            if (value) {
                generator.resume = res; 
                ret({done: false, value});
            } else {
                ret({done: true});
            }
        });
    };
    ret(generator);
}

