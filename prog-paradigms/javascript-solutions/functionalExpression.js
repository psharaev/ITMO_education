"use strict";

const expression = (func) => (...operands) => (...values) => {
    return func(...operands.map((operand) => operand(...values)));
};

const cnst = (val) => () => val;

const one = cnst(1);
const two = cnst(2);

const variable = (name) => (...values) => {
    switch (name) {
        case "x":
            return values[0];
        case "y":
            return values[1];
        case "z":
            return values[2];
        default:
            return undefined;
    }
};

const add = expression((a, b) => a + b);

const subtract = expression((a, b) => a - b);

const multiply = expression((a, b) => a * b);

const divide = expression((a, b) => a / b);

const negate = expression((a) => -a);

const exampleExp = add(
    subtract(
        multiply(
            variable("x"),
            variable("x")
        ),
        multiply(
            cnst(2),
            variable("x")
        )
    ),
    cnst(1)
);

for (let i = 0; i < 10; i++) {
    println(exampleExp(i));
}