// Recursive descent calculator
export function calc(input, ret, thro) {
    expression(input, result => {
        match(input, 'END', b => {
            if (b) {
                ret(result);
            } else {
                thro(`Unexpected token: ${currentToken}`);
            }
        });
    }, thro);
}

function expression(input, ret, thro) {
    term(input,
        left => expressionRest(input, left, ret, thro),
        thro
    );
}

function expressionRest(input, left, ret, thro) {
    match(input, 'PLUS', b => {
        if (b) {
            term(input,
                right => expressionRest(input, left + right, ret, thro),
                thro
            );
        } else {
            match(input, 'MINUS', b => {
                if (b) {
                    term(input,
                        right => expressionRest(input, left - right, ret, thro),
                        thro
                    );
                } else {
                    ret(left);
                }
            });
        }
    });
}

function term(input, ret, thro) {
    factor(input,
        left => termRest(input, left, ret, thro),
        thro
    );
}

function termRest(input, left, ret, thro) {
    match(input, 'TIMES', b => {
        if (b) {
            factor(input,
                right => termRest(input, left * right, ret, thro),
                thro
            );
        } else {
            match(input, 'DIVIDE', b => {
                if (b) {
                    factor(input,
                        right => termRest(input, left / right, ret, thro),
                        thro
                    );
                } else {
                    ret(left);
                }
            });
        }
    });
}

function factor(input, ret, thro) {
    match(input, 'ID', b => {
        if (b) {
            lookup(previousLexeme, ret);
        } else {
            match(input, 'NUM', b => {
                if (b) {
                    ret(Number(previousLexeme));
                } else {
                    match(input, 'LPAREN', b => {
                        if (b) {
                            expression(input,
                                result => {
                                    match(input, 'RPAREN', b => {
                                        if (b) {
                                            ret(result);
                                        } else {
                                            thro(`Unexpected token: ${currentToken}`);
                                        }
                                    });
                                },
                                thro
                            );
                        } else {
                            thro(`Unexpected token: ${currentToken}`);
                        }
                    });
                }
            });
        }
    });
}

// This is a placeholder for a proper environment lookup
function lookup(id, ret) {
    if (id === 'answer') {
        ret(42);
    } else {
        ret(0);
    }
}

let currentToken = undefined;
let currentLexeme = undefined;
let previousToken = undefined;
let previousLexeme = undefined;

function advance(input, ret) {
    input.next(current => {
        previousToken = currentToken;
        previousLexeme = currentLexeme;

        if (current.done) {
            currentToken = 'END';
            currentLexeme = undefined;
        } else {
            currentToken = current.value.token;
            currentLexeme = current.value.lexeme;
        }
        ret();
    });
}

function match(input, token, ret) {
    if (!currentToken) {
        advance(input,
            () => {
                if (currentToken === token) {
                    advance(input, () => ret(true));
                } else {
                    ret(false);
                }
            }
        );
    }
    if (currentToken === token) {
        advance(input, () => ret(true));
    } else {
        ret(false);
    }
}