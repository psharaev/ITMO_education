package expression.exceptions;

import expression.Operand;

public class CheckedNegate implements Operand {

    private final Operand operand;

    public CheckedNegate(final Operand operand) {
        this.operand = operand;
    }

    public int calc(final int a) {
        if (a == Integer.MIN_VALUE) {
            throw new ArithmeticException("int overflow");
        }
        return -a;
    }

    @Override
    public int evaluate(int x) {
        return calc(operand.evaluate(x));
    }

    @Override
    public int evaluate(final int x, final int y, final int z) {
        return calc(operand.evaluate(x, y, z));
    }

    @Override
    public String toString() {
        return "-" + operand.toString();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj != null && obj.getClass() == CheckedNegate.class) {
            return ((CheckedNegate) obj).operand.equals(this.operand);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return operand.hashCode() * 31;
    }
}
