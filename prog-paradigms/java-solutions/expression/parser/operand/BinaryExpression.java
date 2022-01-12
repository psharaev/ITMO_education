package expression.parser.operand;

import expression.calculator.Calculator;

public abstract class BinaryExpression<T> implements GenericOperand<T> {

    private final String operator;

    private final GenericOperand<T> left;
    private final GenericOperand<T> right;

    public BinaryExpression(final String operator, final GenericOperand<T> left, final GenericOperand<T> right) {
        this.operator = operator;
        this.left = left;
        this.right = right;
    }

    public abstract T calc(final Calculator<T> calculator, final T a, final T b);

    @Override
    public T evaluate(final Calculator<T> calculator, final T x, final T y, final T z) {
        return calc(calculator, left.evaluate(calculator, x, y, z), right.evaluate(calculator, x, y, z));
    }

    @Override
    public String toString() {
        return String.format("(%s %s %s)", left.toString(), operator, right.toString());
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof BinaryExpression<?>) {
            BinaryExpression<?> that = (BinaryExpression<?>) obj;
            return this.getClass() == that.getClass() && left.equals(that.left) && right.equals(that.right);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return (31 * left.hashCode() + right.hashCode()) * 31 + getClass().hashCode();
    }
}