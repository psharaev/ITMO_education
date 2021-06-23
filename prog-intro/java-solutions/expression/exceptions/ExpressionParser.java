package expression.exceptions;

import expression.*;
import expression.parser.BaseParser;
import expression.parser.StringSource;

public class ExpressionParser extends BaseParser implements Parser {

    /*

    E - expression + -
    T - term * /
    F - fundamental
    U - unary minus

    C - const
    V - variable

    E -> T + E | T - E | T
    T -> F * T | F / T | F
    F -> C     | V     | FUNC F  | (L)   | U
    U -> -C    | -V    | -(E)

     */

    public Operand parse(String expression) {
        this.source = new StringSource(expression);
        nextChar();
        Operand result = parseE();
        if (eof()) {
            return result;
        }

        throw error("Expected end of input, actual: '" + ch + '\'');
    }

    private Operand parseE() {
        Operand left = parseT();

        while (!test(END)) {
            skipWhiteSpace();
            char op = ch;
            if (op != '+' && op != '-') {
                break;
            }
            nextChar();

            Operand right = parseT();
            if (op == '+') {
                left = new CheckedAdd(left, right);
            } else if (op == '-') {
                left = new CheckedSubtract(left, right);
            }
        }

        return left;
    }

    private Operand parseT() {
        Operand left = parseF();

        while (!test(END)) {
            skipWhiteSpace();
            char op = ch;
            if (op != '*' && op != '/') {
                break;
            }
            nextChar();

            Operand right = parseF();
            if (op == '*') {
                left = new CheckedMultiply(left, right);
            } else if (op == '/') {
                left = new CheckedDivide(left, right);
            }
        }

        return left;
    }

    private Operand parseF() {
        skipWhiteSpace();

        if (isDigit()) {
            return parseConst();
        } else if (test('(')) {
            Operand lowestLevel = parseE();
            expect(')');
            return lowestLevel;
        } else if (ch == '-') {
            return parseU();
        } else if (isLetter()) {
            String name = readName();
            if (name.equals("sqrt")) {
                return new Sqrt(parseF());
            } else if (name.equals("abs")) {
                return new Abs(parseF());
            }
            return new Variable(name);
        }

        throw error("Expected constant, variable or parentheses, actual: '" + ch + '\'');
    }

    private Operand parseU() {

        expect('-');

        skipWhiteSpace();
        if (isDigit()) {
            return parseNegativeConst();
        } else if (test('(')) {
            Operand nestedOperand = parseE();
            expect(')');
            return new CheckedNegate(nestedOperand);
        } else if (isLetter()) {
            String name = readName();
            if (name.equals("sqrt")) {
                return new CheckedNegate(new Sqrt(parseF()));
            } else if (name.equals("abs")) {
                return new CheckedNegate(new Abs(parseF()));
            }
            return new CheckedNegate(new Variable(name));
        }
        return new CheckedNegate(parseU());
    }

    private Operand parseConst() {
        String value = readDigits();
        return new Const(Integer.parseInt(value));
    }

    private Operand parseNegativeConst() {
        String value = readDigits();
        return new Const(Integer.parseInt('-' + value));
    }


    private void skipWhiteSpace() {
        while (Character.isWhitespace(ch)) {
            nextChar();
        }
    }

    private String readName() {
        final StringBuilder sb = new StringBuilder();

        do {
            sb.append(ch);
            nextChar();
        } while (isLetter());

        String s = sb.toString();
        if (s.equals("x") || s.equals("y") || s.equals("z") || s.equals("abs") || s.equals("sqrt"))
            return s;

        throw error("Unexpected variable or function name: " + s);
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