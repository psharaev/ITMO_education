"use strict"

const expression = function () {

    const Const = function (c) {
        this.c = c;
    }
    Const.prototype = {
        evaluate: function (x, y, z) {
            return this.c;
        },
        toString: function () {
            return this.c.toString();
        },
        prefix: function () {
            return this.c.toString();
        }
    }

    const Variable = function (name) {
        this.name = name;
    }
    Variable.prototype = {
        evaluate: function (x, y, z) {
            switch (this.name) {
                case "x":
                    return x;
                case "y":
                    return y;
                case "z":
                    return z;
                default:
                    return undefined;
            }
        },
        toString: function () {
            return this.name;
        },
        prefix: function () {
            return this.name;
        }
    }

    const ExpressionFactory = function (operator, countArguments, calc) {
        const Expression = function (...operands) {
            this.operands = operands;
        }

        Expression.prototype = {
            evaluate: function (x, y, z) {
                const evaluated = this.operands.map((operand) => operand.evaluate(x, y, z));
                return this.calc(...evaluated);
            },
            toString: function () {
                return this.operands.join(" ") + " " + this.operator;
            },
            prefix: function () {
                return "(" + this.operator + " " + this.operands.map((operand) => operand.prefix()).join(" ") + ")";
            },
            calc: calc,
            countArguments: countArguments,
            operator: operator,
        }

        return Expression;
    };

    const Add = ExpressionFactory("+", 2, (a, b) => a + b);
    const Subtract = ExpressionFactory("-", 2, (a, b) => a - b);
    const Multiply = ExpressionFactory("*", 2, (a, b) => a * b);
    const Divide = ExpressionFactory("/", 2, (a, b) => a / b);
    const Negate = ExpressionFactory("negate", 1, (a) => -a);

    const Avg5 = ExpressionFactory("avg5", 5, (a, b, c, d, e) => (a + b + c + d + e) / 5);
    const Med3 = ExpressionFactory("med3", 3, (a, b, c) => [a, b, c].sort((a, b) => a - b)[1]);

    const Sinh = ExpressionFactory("sinh", 1, (a) => Math.sinh(a));
    const Cosh = ExpressionFactory("cosh", 1, (a) => Math.cosh(a));

    const ArithMean = ExpressionFactory("arith-mean", -1, (...arr) => {
        let sum = 0;
        for (const item of arr) {
            sum += item;
        }
        return sum / arr.length;
    });
    const GeomMean = ExpressionFactory("geom-mean", -1, (...arr) => {
        let mul = 1;
        for (const item of arr) {
            mul *= item;
        }
        return Math.pow(Math.abs(mul), 1 / arr.length);
    });
    const HarmMean = ExpressionFactory("harm-mean", -1, (...arr) => {
        let sum = 0;
        for (const item of arr) {
            sum += 1 / item;
        }
        return arr.length / sum;
    });

    const parsePrefix = function (s) {
        let pos = 0;
        let ch = "";
        const END = '\0';
        const declaredFunctions = new Map([
            ["+", Add],
            ["-", Subtract],
            ["*", Multiply],
            ["/", Divide],
            ["sinh", Sinh],
            ["cosh", Cosh],
            ["negate", Negate],
            ["avg5", Avg5],
            ["med3", Med3],
            ["arith-mean", ArithMean],
            ["geom-mean", GeomMean],
            ["harm-mean", HarmMean]
        ]);

        const exceptions = function () {
            const factory = function (message) {
                const Exception = function (token) {
                    this.message = message + ", pos: " + pos + ", actual: '" + token + "'";
                };
                Exception.prototype = new Error();
                return Exception;
            };

            const wrongCountFactory = function (message) {
                const Exception = function (funcName, expected, actual) {
                    this.message = message + ": '" + funcName + "', expected: " + expected + ", actual: " + actual;
                };
                Exception.prototype = new Error();
                return Exception;
            }

            const unknownFactory = function (message) {
                const Exception = function (unknown) {
                    this.message = message + ": '" + unknown + "', pos: " + pos;
                };
                Exception.prototype = new Error();
                return Exception;
            }

            const MissCloseParenthesisException = factory("Closing parenthesis expected");
            const EndOfInputExpectedException = factory("End of expression expected");
            const OperationExpectedException = factory("Operation symbol expected");
            const WrongNumberOfArgumentsException = wrongCountFactory("Wrong number of arguments");
            const UnknownFunctionException = unknownFactory("Unknown function");
            const UnknownSymbolException = unknownFactory("Unknown symbol");

            return {
                MissCloseParenthesisException: MissCloseParenthesisException,
                EndOfInputExpectedException: EndOfInputExpectedException,
                OperationExpectedException: OperationExpectedException,
                WrongNumberOfArgumentsException: WrongNumberOfArgumentsException,
                UnknownFunctionException: UnknownFunctionException,
                UnknownSymbolException: UnknownSymbolException
            }
        }();

        function nextChar() {
            ch = pos < s.length ? s[pos++] : END;
        }

        function test(expected) {
            if (ch === expected) {
                nextChar();
                return true;
            }
            return false;
        }

        function skipWhiteSpace() {
            while (/\s/.test(ch)) {
                nextChar();
            }
        }

        function parseExpression() {
            let res = "";
            while (ch !== END && ch !== '(' && ch !== ')' && /\S/.test(ch)) {
                res += ch;
                nextChar();
            }
            return res;
        }

        function nextToken() {
            skipWhiteSpace();

            if (test('(')) {
                skipWhiteSpace();

                const op = parseExpression();

                const constructor = declaredFunctions.get(op);
                if (constructor !== undefined) {
                    const parsed = [];

                    skipWhiteSpace();
                    while (!test(')')) {
                        if (test(END)) {
                            throw new exceptions.MissCloseParenthesisException(ch);
                        }

                        parsed.push(nextToken());
                        skipWhiteSpace();
                    }

                    if (constructor.prototype.countArguments !== -1 && constructor.prototype.countArguments !== parsed.length) {
                        throw new exceptions.WrongNumberOfArgumentsException(op, constructor.prototype.countArguments, parsed.length);
                    }

                    return new constructor(...parsed);
                }

                throw new exceptions.UnknownFunctionException(op);
            } else {
                return parseSymbol(parseExpression());
            }
        }

        function parseSymbol(s) {
            if (s === "x" || s === "y" || s === "z") {
                return new Variable(s);
            }

            if (!isNaN(s)) {
                return new Const(Number.parseInt(s));
            }

            throw new exceptions.UnknownSymbolException(s);
        }

        if (s === "") {
            throw new Error("empty string");
        }

        nextChar();
        const expr = nextToken();
        skipWhiteSpace();
        if (test(END)) {
            return expr;
        }

        throw new exceptions.EndOfInputExpectedException(ch);
    };

    return {
        Const: Const,
        Variable: Variable,

        Add: Add,
        Subtract: Subtract,
        Multiply: Multiply,
        Divide: Divide,
        Negate: Negate,

        Sinh: Sinh,
        Cosh: Cosh,

        Avg5: Avg5,
        Med3: Med3,

        ArithMean: ArithMean,
        GeomMean: GeomMean,
        HarmMean: HarmMean,

        parsePrefix: parsePrefix,
    };
}();

const Const = expression.Const;
const Variable = expression.Variable;

const Add = expression.Add;
const Subtract = expression.Subtract;
const Multiply = expression.Multiply;
const Divide = expression.Divide;
const Negate = expression.Negate;

const Sinh = expression.Sinh;
const Cosh = expression.Cosh;

const Avg5 = expression.Avg5;
const Med3 = expression.Med3;

const ArithMean = expression.ArithMean;
const GeomMean = expression.GeomMean;
const HarmMean = expression.HarmMean;

const parsePrefix = expression.parsePrefix;