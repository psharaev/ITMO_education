package expression.exceptions;

import expression.BinaryExpression;
import expression.Operand;

public class CheckedDivide extends BinaryExpression {

    public CheckedDivide(final Operand left, final Operand right) {
        super("/", left, right);
    }

    @Override
    public int calc(final int a, final int b) {
        if (b == 0) {
            throw new ArithmeticException("division by zero");
        }
        if (a == Integer.MIN_VALUE && b == -1) {
            throw new ArithmeticException("int overflow");
        }
        return a / b;
    }
}
