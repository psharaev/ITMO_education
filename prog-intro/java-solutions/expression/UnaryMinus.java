package expression;

public class UnaryMinus implements Operand {
    private final Operand operand;

    public UnaryMinus(final Operand operand) {
        this.operand = operand;
    }

    public int calc(final int a) {
        return -a;
    }

    @Override
    public int evaluate(final int num) {
        return calc(operand.evaluate(num));
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
        if (obj != null && obj.getClass() == UnaryMinus.class) {
            return ((UnaryMinus) obj).operand.equals(this.operand);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return operand.hashCode() * 31;
    }
}
