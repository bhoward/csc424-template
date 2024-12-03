const { run, Rel, eq, conde, exist, conso, firsto, resto, nilo } = require('ramo');

// Expressions
function makeVar(name) {
    return { tag: "var", name };
}

function makeNum(value) {
    return { tag: "num", value };
}

function makeApply(fun, arg) {
    return { tag: "app", fun, arg };
}

function makeLambda(param, body) {
    return { tag: "lam", param, body };
}

// Types
const IntType = { tag: "int" };

function makeFun(arg, result) {
    return { tag: "fun", arg, result };
}

// Bindings
function makeBind(name, type) {
    return { name, type };
}

// Relations
const lookupo = Rel((env, name, type) => conde(
    firsto(env, makeBind(name, type)),
    exist(rest => [
        resto(env, rest),
        lookupo(rest, name, type)
    ])
));

const typeo = Rel((env, expr, t) => conde(
    exist(name => [
        eq(expr, makeVar(name)),
        lookupo(env, name, t)
    ]),
    exist(value => [
        eq(expr, makeNum(value)),
        eq(t, IntType)
    ]),
    exist((fun, arg, argT) => [
        eq(expr, makeApply(fun, arg)),
        typeo(env, arg, argT),
        typeo(env, fun, makeFun(argT, t))
    ]),
    exist((param, body, t1, t2, env2) => [
        eq(expr, makeLambda(param, body)),
        eq(t, makeFun(t1, t2)),
        conso(makeBind(param, t1), env, env2),
        typeo(env2, body, t2)
    ])
));

const id = makeLambda("x", makeVar("x"));

const idT = run()(t => typeo([], id, t));

console.log(idT);

const base = [
    makeBind("+", makeFun(IntType, makeFun(IntType, IntType))),
];

const succ = makeLambda("n", makeApply(makeApply(makeVar("+"), makeVar("n")), makeNum(1)));

const succT = run()(t => typeo(base, succ, t));

console.log(succT);