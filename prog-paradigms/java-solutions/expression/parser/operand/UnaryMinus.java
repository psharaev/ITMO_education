package expression.parser.operand;

import expression.calculator.Calculator;

public class UnaryMinus<T> implements GenericOperand<T> {
    private final GenericOperand<T> operand;

    public UnaryMinus(final GenericOperand<T> operand) {
        this.operand = operand;
    }

    public T calc(final Calculator<T> calculator, final T a) {
        return calculator.negate(a);
    }

    @Override
    public T evaluate(final Calculator<T> calculator, final T x, final T y, final T z) {
        return calc(calculator, operand.evaluate(calculator, x, y, z));
    }

    @Override
    public String toString() {
        return "-" + operand.toString();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj != null && obj.getClass() == UnaryMinus.class) {
            return ((UnaryMinus<?>) obj).operand.equals(this.operand);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return operand.hashCode() * 31;
    }
}
