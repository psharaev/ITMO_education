package expression.exceptions;

import expression.BinaryExpression;
import expression.Operand;
import expression.TripleExpression;
import expression.Variable;

public class CheckedSubtract extends BinaryExpression {

    public CheckedSubtract(final Operand left, final Operand right) {
        super("-", left, right);
    }

    @Override
    public int calc(final int a, final int b) {
        int r = a - b;
        if (((a ^ b) & (a ^ r)) < 0) {
            throw new ArithmeticException("int overflow");
        }
        return r;
    }
}
