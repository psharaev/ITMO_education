package expression.parser.operand;

import expression.calculator.Calculator;

public class Add<T> extends BinaryExpression<T> {

    public Add(final GenericOperand<T> left, final GenericOperand<T> right) {
        super("+", left, right);
    }

    @Override
    public T calc(final Calculator<T> calculator, final T a, final T b) {
        return calculator.add(a, b);
    }
}
