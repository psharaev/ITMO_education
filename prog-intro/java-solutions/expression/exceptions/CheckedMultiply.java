package expression.exceptions;

import expression.BinaryExpression;
import expression.Operand;

public class CheckedMultiply extends BinaryExpression {

    public CheckedMultiply(final Operand left, final Operand right) {
        super("*", left, right);
    }

    @Override
    public int calc(final int a, final int b) {
        int r = a * b;
        if (((b != 0) && (r / b != a)) || (a == Integer.MIN_VALUE && b == -1)) {
            throw new ArithmeticException("int overflow");
        }
        return r;
    }
}
