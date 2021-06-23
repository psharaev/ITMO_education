package expression;

public class Abs implements Operand {
    private final Operand operand;

    public Abs(final Operand operand) {
        this.operand = operand;
    }

    public int calc(final int a) {
        if (a == Integer.MIN_VALUE) {
            throw new ArithmeticException("integer overflow");
        }
        return a >= 0 ? a : -a;
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
        return "abs(" + operand.toString() + ")";
    }

    @Override
    public boolean equals(Object obj) {
        if (obj != null && obj.getClass() == Abs.class) {
            return ((Abs) obj).operand.equals(this.operand);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return operand.hashCode() * 31;
    }
}