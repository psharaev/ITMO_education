package expression.parser.operand;

import expression.calculator.Calculator;

public class Subtract<T> extends BinaryExpression<T> {

    public Subtract(final GenericOperand<T> left, final GenericOperand<T> right) {
        super("-", left, right);
    }

    @Override
    public T calc(final Calculator<T> calculator, final T a, final T b) {
        return calculator.subtract(a, b);
    }
}
