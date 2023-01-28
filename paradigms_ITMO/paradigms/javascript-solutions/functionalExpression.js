"use strict";

const operation = ops => {
    let curr = (...args) => (...vars) => ops(...args.map(op => op(...vars)));
    curr.len = ops.length;
    return curr;
};
const variable = name => (...vars) => vars[masVar.indexOf(name)];
const cnst = value => () => +value;

const one = cnst(1);
const two = cnst(2);
const argToCnst = {
    "one": one,
    "two": two
};

const masVar = [
    "x",
    "y",
    "z"
];  

const add = operation((a, b) => a + b);
const subtract = operation((a, b) => a - b);
const multiply = operation((a, b) => a * b);
const divide = operation((a, b) => a / b);
const negate = operation(a => -a);
const max3 = operation((a, b, c) => Math.max(a, b, c));
const min5 = operation((a, b, c, d, e) => Math.min(a, b, c, d, e));
const argToOperation = {
    "+": add,
    "-": subtract,
    "*": multiply,
    "/": divide,
    "negate": negate,
    "max3": max3,
    "min5": min5
};

function parse(expression) {
    const args = expression.trim().split(/ +/);
    let ma = [];
    for (let arg of args) {
        if (arg in argToCnst) {
            ma.push(argToCnst[arg]);
        } 
        else if (masVar.includes(arg)) {
            ma.push(variable(arg));
        }
        else if (arg in argToOperation) {
            const curr = argToOperation[arg];
            ma.push(curr(...ma.splice(-curr.len)));
        }         
        else {
            ma.push(cnst(arg));
        }
    }
    return ma.pop();
}
