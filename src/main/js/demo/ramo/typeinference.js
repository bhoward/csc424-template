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

function printExpr(expr) {
    switch (expr.tag) {
        case "var": return expr.name;
        case "num": return expr.value;
        case "app": return `(${printExpr(expr.fun)}(${printExpr(expr.arg)}))`;
        case "lam": return `(${expr.param} => ${printExpr(expr.body)})`;
    }
}

// Types
const IntType = { tag: "int" };

function makeFun(arg, result) {
    return { tag: "fun", arg, result };
}

function printType(type) {
    switch (type.tag) {
        case "int": return "int";
        case "fun": return `(${printType(type.arg)} -> ${printType(type.result)})`;
        default: return type;
    }
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

// Parsing
// S -->  Fun $
// Fun -->  Var => Fun  |  Expr
// Expr -->  Expr + Term  |  Term
// Term -->  Term * Factor  |  Factor
// Factor -->  Var  |  Num  |  Factor ( Fun )  |  ( Fun )
const parseo = Rel((source, expr) =>
    exist(rest => [
        funo(source, expr, rest),
        nilo(rest)
    ])
);

const funo = Rel((source, expr, rest) => conde(
    exist((param, body, r1, r2) => [
        eq(expr, makeLambda(param, body)),
        varo(source, makeVar(param), r1),
        matcho(r1, "=>", r2),
        funo(r2, body, rest)
    ]),
    expro(source, expr, rest)
));

const expro = Rel((source, expr, rest) =>
    exist((left, r1) => [
        termo(source, left, r1),
        eresto(r1, left, expr, rest)
    ])
);

const eresto = Rel((source, left, expr, rest) => conde(
    exist((right, app, r1, r2) => [
        matcho(source, "+", r1),
        termo(r1, right, r2),
        eresto(r2, app, expr, rest),
        eq(app, makeApply(makeApply(makeVar("+"), left), right))
    ]),
    [
        eq(source, rest),
        eq(left, expr)
    ]
));

const termo = Rel((source, expr, rest) =>
    exist((left, r1) => [
        factoro(source, left, r1),
        tresto(r1, left, expr, rest)
    ])
);

const tresto = Rel((source, left, expr, rest) => conde(
    exist((right, app, r1, r2) => [
        matcho(source, "*", r1),
        factoro(r1, right, r2),
        tresto(r2, app, expr, rest),
        eq(app, makeApply(makeApply(makeVar("*"), left), right))
    ]),
    [
        eq(source, rest),
        eq(left, expr)
    ]
));

const factoro = Rel((source, expr, rest) => conde(
    exist((left, r1) => [
        varo(source, left, r1),
        fresto(r1, left, expr, rest)
    ]),
    exist((left, r1) => [
        numo(source, left, r1),
        fresto(r1, left, expr, rest)
    ]),
    exist((left, r1, r2, r3) => [
        matcho(source, "(", r1),
        funo(r1, left, r2),
        matcho(r2, ")", r3),
        fresto(r3, left, expr, rest)
    ])
));

const fresto = Rel((source, left, expr, rest) => conde(
    exist((right, app, r1, r2, r3) => [
        matcho(source, "(", r1),
        funo(r1, right, r2),
        matcho(r2, ")", r3),
        fresto(r3, app, expr, rest),
        eq(app, makeApply(left, right))
    ]),
    [
        eq(source, rest),
        eq(left, expr)
    ]
));

const varo = Rel((source, expr, rest) =>
    exist((v) => [
        matcho(source, v, rest),
        conde(
            eq(v, "a"),
            eq(v, "b"),
            eq(v, "c"),
            eq(v, "d"),
            // Need to generalize this...
        ),
        eq(expr, makeVar(v))
    ])
);

const numo = Rel((source, expr, rest) =>
    exist((n) => [
        matcho(source, n, rest),
        conde(
            eq(n, "0"),
            eq(n, "1"),
            eq(n, "2"),
            eq(n, "3"),
            // Need to generalize this...
        ),
        eq(expr, makeNum(n))
    ])
);

const matcho = Rel((source, s, rest) =>
    conso(s, rest, source)
);

// Demos
const id = makeLambda("a", makeVar("a"));
console.log(printExpr(id), " has type:");
const idT = run()(t => typeo([], id, t))[0];
console.log(printType(idT));
console.log();

const base = [
    makeBind("+", makeFun(IntType, makeFun(IntType, IntType))),
    makeBind("*", makeFun(IntType, makeFun(IntType, IntType))),
];

const succ = makeLambda("a", makeApply(makeApply(makeVar("+"), makeVar("a")), makeNum("1")));
console.log(printExpr(succ), " has type:")
const succT = run()(t => typeo(base, succ, t))[0];
console.log(printType(succT));
console.log();

const input = ["a", "=>", "a", "+", "1"];
console.log(input, " parses to:");
const succ2 = run()(e => parseo(input, e))[0];
console.log(printExpr(succ2));
console.log();

console.log("Five terms with type ", printType(idT), " are:");
const ids = run(5)(e => typeo(base, e, idT));
for (const e of ids) {
    console.log(printExpr(e));
}
console.log();

console.log("Unparsing ", printExpr(id), " produces:");
const pid = run(1)(s => parseo(s, id))[0];
console.log(pid);
console.log();

console.log("Unparsing ", printExpr(succ), " produces:");
const psucc = run(1)(s => parseo(s, succ))[0];
console.log(psucc);
console.log();