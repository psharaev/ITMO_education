package expression.parser.operand;

import expression.calculator.Calculator;

public class Multiply<T> extends BinaryExpression<T> {

    public Multiply(final GenericOperand<T> left, final GenericOperand<T> right) {
        super("*", left, right);
    }

    @Override
    public T calc(final Calculator<T> calculator, final T a, final T b) {
        return calculator.multiply(a, b);
    }
}
