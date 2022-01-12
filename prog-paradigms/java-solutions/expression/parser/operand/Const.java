package expression.parser.operand;

import expression.calculator.Calculator;

public class Const<T> implements GenericOperand<T> {

    private final T constant;

    public Const(final T constant) {
        this.constant = constant;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj != null && obj.getClass() == Const.class) {
            return ((Const<?>) obj).constant.equals(this.constant);
        }
        return false;
    }

    @Override
    public T evaluate(Calculator<T> calculator, T x, T y, T z) {
        return constant;
    }

    @Override
    public String toString() {
        return constant.toString();
    }

    @Override
    public int hashCode() {
        return constant.hashCode();
    }

}
