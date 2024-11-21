// Recursive descent calculator
export function calc(input) {
    let result = expression(input);
    if (match(input, 'END')) {
        return result;
    } else {
        throw `Unexpected token: ${currentToken}`;
    }
}

function expression(input) {
    let left = term(input);
    return expressionRest(input, left);
}

function expressionRest(input, left) {
    if (match(input, 'PLUS')) {
        let right = term(input);
        return expressionRest(input, left + right);
    } else if (match(input, 'MINUS')) {
        let right = term(input);
        return expressionRest(input, left - right);
    } else {
        return left;
    }
}

function term(input) {
    let left = factor(input);
    return termRest(input, left);
}

function termRest(input, left) {
    if (match(input, 'TIMES')) {
        let right = factor(input);
        return termRest(input, left * right);
    } else if (match(input, 'DIVIDE')) {
        let right = factor(input);
        return termRest(input, left / right);
    } else {
        return left;
    }
}

function factor(input) {
    if (match(input, 'ID')) {
        const id = previousLexeme;
        return lookup(id);
    } else if (match(input, 'NUM')) {
        const num = Number(previousLexeme);
        return num;
    } else if (match(input, 'LPAREN')) {
        let result = expression(input);
        if (match(input, 'RPAREN')) {
            return result;
        } else {
            throw `Unexpected token: ${currentToken}`;
        }
    } else {
        throw `Unexpected token: ${currentToken}`;
    }
}

// This is a placeholder for a proper environment lookup
function lookup(id) {
    if (id === 'answer') {
        return 42;
    } else {
        return 0;
    }
}

let currentToken = undefined;
let currentLexeme = undefined;
let previousToken = undefined;
let previousLexeme = undefined;

function advance(input) {
    const current = input.next();
    previousToken = currentToken;
    previousLexeme = currentLexeme;

    if (current.done) {
        currentToken = 'END';
        currentLexeme = undefined;
    } else {
        currentToken = current.value.token;
        currentLexeme = current.value.lexeme;
    }
}

function match(input, token) {
    if (!currentToken) {
        advance(input);
    }
    if (currentToken === token) {
        advance(input);
        return true;
    } else {
        return false;
    }
}