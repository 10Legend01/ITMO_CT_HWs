"use strict";

const TypeFactory = (main, evaluate, diff, toString, simplify) => {
    main.prototype.evaluate = evaluate;
    main.prototype.diff = diff;
    main.prototype.toString = toString;
    main.prototype.simplify = simplify;
    main.prototype.prefix = toString;
    main.prototype.postfix = toString;
    return main;
}

const Const = TypeFactory(
    function(val) { this.val = val },
    function() { return this.val },
    function() { return Const.ZERO },
    function() { return this.val.toString() },
    function() { return this }
)

Const.ZERO = new Const(0);
Const.ONE = new Const(1);
Const.TWO = new Const(2);
Const.THREE = new Const(3);

const masVar = [
    "x",
    "y",
    "z"
]

let argToOperation = {}

const Variable = TypeFactory(
    function(name) {
        this.name = name;
        this.index = masVar.indexOf(name);
    },
    function(...vars) { return vars[this.index] },
    function(name) { return name === this.name ? Const.ONE : Const.ZERO },
    function() { return this.name },
    function() { return this }
)

const Operation = {
    evaluate: function(...vars) { return this.ops(...this.args.map(op => op.evaluate(...vars))) },
    diff: function(name) { return this.getDiff(name, ...this.args.map(op => [op, op.diff(name)])) },
    toString: function() { return this.args.join(' ') + ' ' + this.sign },
    simplify: function() { return this.getSimplify(...this.args.map(x => x.simplify())) },
    prefix: function () { return "(" + this.sign + " " + this.args.map(x => x.prefix()).join(" ") + ")" },
    postfix: function () { return "(" + this.args.map(x => x.postfix()).join(" ") + " " + this.sign + ")" }
}

const makeOperation = (ops, getDiff, sign, getSimplify) => {
    let Op = function(...args) {
        this.args = args;
    };
    Op.prototype = Object.create(Operation);
    Op.prototype.ops = ops;
    Op.prototype.getDiff = getDiff;
    Op.prototype.sign = sign;
    Op.prototype.getSimplify = getSimplify;
    Op.len = ops.length;
    argToOperation[sign] = Op;
    return Op;
}

const toSimplify = (op, [...args], [...functions]) => {
    for (let [a, zn, b] of functions) {
        if (a.val === zn) {
            return b;
        }
    }
    if (args.filter(arg => arg.val == null).length > 0) {
        return new op(...args);
    }
    return new Const(op.prototype.ops(...args));
}

const Add = makeOperation(
    (a, b) => a + b,
    (name, [a, ad], [b, bd]) => new Add(ad, bd),
    "+",
    (a, b) => toSimplify(
        Add,
        [a, b],
        [
            [a, 0, b],
            [b, 0, a]
        ]
    )
)

const Subtract = makeOperation(
    (a, b) => a - b,
    (name, [a, ad], [b, bd]) => new Subtract(ad, bd),
    "-",
    (a, b) => toSimplify(
        Subtract,
        [a, b],
        [
            [b, 0, a]
        ]
    )
)

const Multiply = makeOperation(
    (a, b) => a * b,
    (name, [a, ad], [b, bd]) => new Add(
        new Multiply(a, bd),
        new Multiply(ad, b)),
    "*",
    (a, b) => toSimplify(
        Multiply,
        [a, b],
        [
            [a, 0, Const.ZERO],
            [b, 0, Const.ZERO],
            [a, 1, b],
            [b, 1, a]
        ]
    )
)

const Divide = makeOperation(
    (a, b) => a / b,
    (name, [a, ad], [b, bd]) => new Divide(
        new Subtract(
            new Multiply(ad, b),
            new Multiply(a, bd)),
        new Multiply(b, b)),
    "/",
    (a, b) => toSimplify(
        Divide,
        [a, b],
        [
            [a, 0, Const.ZERO],
            [b, 1, a]
        ]
    )
)

const Negate = makeOperation(
    a => -a,
    (name, [a, ad]) => new Negate(ad),
    "negate",
    a => toSimplify(
        Negate,
        [a],
        []
    )
)

const Cube = makeOperation(
    a => a * a * a,
    (name, [a, ad]) => new Multiply(
        new Multiply(
            Const.THREE,
            new Multiply(a, a)),
        ad),
    "cube",
    a => toSimplify(
        Cube,
        [a],
        []
    )
)

const Cbrt = makeOperation(
    a => Math.cbrt(a),
    (name, [a, ad]) => new Multiply(
        new Divide(
            Const.ONE,
            new Multiply(
                Const.THREE,
                new Multiply(new Cbrt(a), new Cbrt(a)))),
        ad),
    "cbrt",
    a => toSimplify(
        Cbrt,
        [a],
        []
    )
)

function parse(expression) {
    const args = expression.trim().split(/\s+/);
    let ma = [];
    for (let arg of args) {
        if (masVar.includes(arg)) {
            ma.push(new Variable(arg));
        }
        else if (arg in argToOperation) {
            const curr = argToOperation[arg];
            ma.push(new curr(...ma.splice(-curr.len)));
        }
        else {
            ma.push(new Const(+arg));
        }
    }
    return ma.pop();
}

// _______________dz7___________________

const Sumsq = makeOperation(
    (...args) => args.length === 0 ? 0 :
        (args.reduce((s, x) => Add.prototype.ops(s, x ** 2), 0)),
    (name, ...args) => args.length === 0 ? Const.ZERO :
        (args.reduce((s, x) => new Add(s, new Multiply(
        new Multiply(
            Const.TWO,
            x[0]), x[1])),
        Const.ZERO)),
    "sumsq",
    null
);

const Length = makeOperation(
    (...args) => args.length === 0 ? 0 : Math.sqrt(Sumsq.prototype.ops(...args)),
    (name, ...args) => args.length === 0 ? Const.ZERO : new Multiply(
        new Divide(
            Const.ONE,
            new Multiply(
                Const.TWO,
                new Length(...args.map(x => x[0]))
            )),
        new Sumsq(...args.map(x => x[0])).diff(name)
    ),
    "length",
    null
);

function ErrorFactory(name, message) {
    function ThisError(...args) {
        this.message = message(...args)
    }
    ThisError.prototype = Object.create(Error.prototype)
    ThisError.prototype.name = name
    ThisError.prototype.constructor = Error
    return ThisError
}

const EmptyInputError = ErrorFactory(
    "EmptyInputError",
    () => "Empty input."
)
const UnexpectedEndOfFileError = ErrorFactory(
    "UnexpectedEndOfFileError",
    (pos, char) => "Expected end of file at pos " + pos +", found: " + char
)
const MissingBracketError = ErrorFactory(
    "MissingBracketError",
    (pos, char) => "Expected ')' at pos " + pos + ", found: " + char
)
const UnrecognizedOperationError = ErrorFactory(
    "UnrecognizedOperationError",
    (pos, char) => "Expected operator, found: '" + char + "' at pos " + pos
)
const UnexpectedArgsNumberError = ErrorFactory(
    "UnexpectedArgsNumberError",
    (pos, operand, actualNumber, expectedNumber) => "Invalid arguments number of " + operand + " at pos "
        + pos + ". Expected: " + expectedNumber + ", found: " + actualNumber
)
const UnexpectedArgError = ErrorFactory(
    "UnexpectedTokenError",
    (pos, foundArg) => "Unexpected argument '" + foundArg + "' at pos " + pos
)

function Parser(expression, delimiters) {
    const string = expression
    let pos = 0

    function nextChar() { return string[pos++] }
    function isDelimiter(char) { return delimiters.includes(char) }

    this.getChar = function () { return string[pos] }
    this.getPos = function () { return pos }
    this.hasNext = function () { return pos < expression.length }

    this.skipWhitespaces = function () {
        while (this.hasNext() && this.getChar().trim() === "") {
            nextChar()
        }
    }
    this.nextArg = function() {
        this.skipWhitespaces()
        let arg = ""
        if (this.hasNext() && isDelimiter(this.getChar())) {
            arg = nextChar()
        } else {
            while (this.hasNext() && !isDelimiter(this.getChar())) {
                arg += nextChar()
            }
        }
        return arg
    }
    this.viewArg = function () {
        let arg = this.nextArg()
        pos -= arg.length
        return arg
    }
}


function parseExpression(expression, mode) {
    let string = expression.trim()
    if (string === "") {
        throw new EmptyInputError()
    }
    const pars = new Parser(string, ["(", ")", " "])
    let ans = parse()
    if (pars.hasNext()) {
        throw new UnexpectedEndOfFileError(pars.getPos(), pars.getChar())
    }
    return ans

    function parse() {
        let arg = pars.nextArg()
        if (arg === '(') {
            let func = parseFunction()
            arg = pars.nextArg()
            if (arg !== ')') {
                throw new MissingBracketError(pars.getPos(), arg !== "" ? arg : "end of file")
            }
            return func
        } else if (masVar.includes(arg)) {
            return new Variable(arg)
        } else if (!isNaN(+arg)) {
            return new Const(+arg)
        } else {
            throw new UnexpectedArgError(pars.getPos(), arg)
        }
    }

    function parseFunction() {
        let args, op
        if (mode === "prefix") {
            op = parseOperation()
            args = parseArgs()
        } else {
            args = parseArgs()
            op = parseOperation()
        }
        const argsLen = op.len
        if (argsLen !== 0 && args.length !== argsLen) {
            throw new UnexpectedArgsNumberError(pars.getPos(), op.prototype.sign, args.length, argsLen);
        }
        return new op(...args)
    }

    function parseOperation() {
        let op = pars.nextArg()
        if (!(op in argToOperation)) {
            throw new UnrecognizedOperationError(pars.getPos(), op)
        }
        return argToOperation[op]
    }

    function parseArgs() {
        let args = []
        while (pars.hasNext() && !isEnd()) {
            args.push(parse())
        }
        return args
    }

    function isEnd() {
        let currArg = pars.viewArg()
        return currArg in argToOperation || currArg === ')'
    }
}

function parsePrefix(string) {
    return parseExpression(string, "prefix")
}

function parsePostfix(string) {
    return parseExpression(string, "postfix")
}




/*
let expr = new Subtract(
    new Multiply(
        new Const(2),
        new Variable("x")
    ),
    new Const(3)
);
*/
/*

//let expr = new Cbrt(new Add(new Variable('x'), new Variable('y')))
let expr = parse('x 2 +').diff('x')
console.log(expr.toString())
expr = expr.simplify()
console.log(expr.toString())
console.log(expr.evaluate(2.00000000000000000000,2.00000000000000000000,0.00000000000000000000));
console.log(expr.toString())
console.log(expr.simplify().toString())
console.log("" + expr.diff("x"));
let x = parse("10");
console.log(x);
/*
for (let i = 0; i <= 10; i++) {
    console.log(i + ": " + expr.evaluate(i));
}
 */