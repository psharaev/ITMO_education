package expression.parser.operand;

import expression.calculator.Calculator;

public class Variable<T> implements GenericOperand<T> {

    private final String var;

    public Variable(final String var) {
        this.var = var;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj != null && obj.getClass() == Variable.class) {
            return ((Variable<?>) obj).var.equals(this.var);
        }
        return false;
    }

    @Override
    public T evaluate(final Calculator<T> calculator, final T x, final T y, final T z) {
        switch (var) {
            case "x":
                return x;
            case "y":
                return y;
            case "z":
                return z;
            default:
                throw new RuntimeException("Undefined symbol");
        }
    }

    @Override
    public String toString() {
        return var;
    }


    @Override
    public int hashCode() {
        return var.hashCode();
    }

}
