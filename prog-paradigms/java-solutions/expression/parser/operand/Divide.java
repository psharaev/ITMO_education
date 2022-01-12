package expression.parser.operand;

import expression.calculator.Calculator;

public class Divide<T> extends BinaryExpression<T> {

    public Divide(final GenericOperand<T> left, final GenericOperand<T> right) {
        super("/", left, right);
    }

    @Override
    public T calc(final Calculator<T> calculator, final T a, final T b) {
        return calculator.divide(a, b);
    }
}
