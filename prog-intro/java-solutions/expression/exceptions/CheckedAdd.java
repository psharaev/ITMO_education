package expression.exceptions;

import expression.BinaryExpression;
import expression.Operand;

public class CheckedAdd extends BinaryExpression {

    public CheckedAdd(final Operand left, final Operand right) {
        super("+", left, right);
    }

    @Override
    public int calc(final int a, final int b) {
        int r = a + b;
        if (((a ^ r) & (b ^ r)) < 0) {
            throw new ArithmeticException("int overflow");
        }
        return r;
    }
}
