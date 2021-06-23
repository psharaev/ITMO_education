package expression.parser;

import expression.*;

public class ExpressionParser extends BaseParser implements Parser {

    /*

    L - logic operation
    E - expression + -
    T - term * /
    F - fundamental
    U - unary minus

    C - const
    V - variable

    L -> E | L | E ^ L | E & L | E
    E -> T + E | T - E | T
    T -> F * T | F / T | F
    F -> C     | V     | (L)   | U
    U -> -C    | -V    | -(L)

     */

    public TripleExpression parse(String expression) {
        this.source = new StringSource(expression);

        nextChar();
        TripleExpression result = parseL();
        if (eof()) {
            return result;
        }

        throw error("Expected end of input, actual: '" + ch + '\'');
    }

    private TripleExpression parseL() {
        return parseLogicOperation('|');
    }

    private TripleExpression parseE() {
        TripleExpression left = parseT();

        while (!test(END)) {
            skipWhiteSpace();
            char op = ch;
            if (op != '+' && op != '-') {
                break;
            }
            nextChar();

            TripleExpression right = parseT();
            if (op == '+') {
                left = new Add((Operand) left, (Operand) right);
            } else if (op == '-') {
                left = new Subtract((Operand) left, (Operand) right);
            }
        }
        return left;
    }

    private TripleExpression parseT() {
        TripleExpression left = parseF();

        while (!test(END)) {
            skipWhiteSpace();
            char op = ch;
            if (op != '*' && op != '/') {
                break;
            }
            nextChar();

            TripleExpression right = parseF();
            if (op == '*') {
                left = new Multiply((Operand) left, (Operand) right);
            } else if (op == '/') {
                left = new Divide((Operand) left, (Operand) right);
            }
        }
        return left;
    }

    private TripleExpression parseF() {
        skipWhiteSpace();

        if (isDigit()) {
            return parseConst();
        } else if (test('(')) {
            TripleExpression lowestLevel = parseL();
            if (!test(')')) {
                throw error("expected ')'");
            }
            return lowestLevel;
        } else if (ch == '-') {
            return parseU();
        } else if (isLetter()) {
            return new Variable(readVariable());
        }

        throw new IllegalStateException("wrong state");
    }

    private TripleExpression parseU() {

        if (!test('-')) {
            throw error("wrong state");
        }

        skipWhiteSpace();
        if (isDigit()) {
            return parseNegativeConst();
        } else if (test('(')) {
            TripleExpression lowestLevel = parseL();
            if (!test(')')) {
                throw error("wrong state");
            }
            return new UnaryMinus((Operand) lowestLevel);
        } else if (isLetter()) {
            return new UnaryMinus(new Variable(readVariable()));
        }
        return new UnaryMinus((Operand) parseU());
    }

    private TripleExpression parseLogicOperation(final char operation) {
        TripleExpression left = getLogicLowerOperation(operation);

        while (!test(END)) {
            skipWhiteSpace();
            char op = ch;
            if (op != operation) {
                break;
            }
            nextChar();

            TripleExpression right = getLogicLowerOperation(operation);
            if (op == '|') {
                left = new LogicOR((Operand) left, (Operand) right);
            } else if (op == '^') {
                left = new LogicXOR((Operand) left, (Operand) right);
            } else if (op == '&') {
                left = new LogicAND((Operand) left, (Operand) right);
            }
        }
        return left;
    }

    private TripleExpression getLogicLowerOperation(final char operation) {
        if (operation == '|') {
            return parseLogicOperation('^');
        } else if (operation == '^') {
            return parseLogicOperation('&');
        } else {
            return parseE();
        }
    }

    private TripleExpression parseConst() {
        String value = readDigits();
        return new Const(Integer.parseInt(value));
    }

    private TripleExpression parseNegativeConst() {
        String value = readDigits();
        return new Const(Integer.parseInt('-' + value));
    }


    private void skipWhiteSpace() {
        while (Character.isWhitespace(ch)) {
            nextChar();
        }
    }

    private String readVariable() {
        final StringBuilder sb = new StringBuilder();

        do {
            sb.append(ch);
            nextChar();
        } while (isLetter());

        return sb.toString();
    }

    private String readDigits() {
        final StringBuilder sb = new StringBuilder();

        do {
            sb.append(ch);
            nextChar();
        } while (isDigit());

        return sb.toString();
    }

    private boolean isLetter() {
        return between('a', 'z') || between('A', 'Z');
    }

    private boolean isDigit() {
        return between('0', '9');
    }
}