package expression.parser;

import expression.calculator.Calculator;
import expression.parser.operand.*;

public class ExpressionParser<T> extends BaseParser {

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

    private final Calculator<T> calculator;

    public ExpressionParser(Calculator<T> calculator) {
        this.calculator = calculator;
    }

    public GenericOperand<T> parse(String expression) {
        this.source = new StringSource(expression);
        nextChar();

        GenericOperand<T> result = parseE();
        if (eof()) {
            return result;
        }

        throw error("Expected end of input, actual: '" + ch + '\'');
    }

    private GenericOperand<T> parseE() {
        return parseBinaryOperation('+', '-');
    }

    private GenericOperand<T> parseT() {
        return parseBinaryOperation('*', '/');
    }

    private GenericOperand<T> parseF() {
        skipWhiteSpace();

        if (isDigit()) {
            return parseConst();
        } else if (test('(')) {
            GenericOperand<T> lowestLevel = parseE();
            expect(')');
            return lowestLevel;
        } else if (ch == '-') {
            return parseU();
        } else if (isLetter()) {
            String name = readName();
            return new Variable<>(name);
        }

        throw error("Expected constant, variable or parentheses, actual: '" + ch + '\'');
    }

    private GenericOperand<T> parseU() {

        expect('-');

        skipWhiteSpace();
        if (isDigit()) {
            return parseNegativeConst();
        } else if (test('(')) {
            GenericOperand<T> nestedOperand = parseE();
            expect(')');
            return new UnaryMinus<>(nestedOperand);
        } else if (isLetter()) {
            String name = readName();
            return new UnaryMinus<>(new Variable<>(name));
        }
        return new UnaryMinus<>(parseU());
    }

    private GenericOperand<T> parseBinaryOperation(char firstOperation, char secondOperation) {
        GenericOperand<T> left = firstOperation == '+' ? parseT() : parseF();

        while (!test(END)) {
            skipWhiteSpace();
            char op = ch;
            if (op != firstOperation && op != secondOperation) {
                break;
            }
            nextChar();

            GenericOperand<T> right = firstOperation == '+' ? parseT() : parseF();
            if (op == '+') {
                left = new Add<>(left, right);
            } else if (op == '-') {
                left = new Subtract<>(left, right);
            } else if (op == '*') {
                left = new Multiply<>(left, right);
            } else if (op == '/') {
                left = new Divide<>(left, right);
            }
        }

        return left;
    }

    private GenericOperand<T> parseConst() {
        String value = readDigits();
        return new Const<>(calculator.valueOf(value));
    }

    private GenericOperand<T> parseNegativeConst() {
        String value = readDigits();
        return new Const<>(calculator.valueOf('-' + value));
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
        if (s.equals("x") || s.equals("y") || s.equals("z"))
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