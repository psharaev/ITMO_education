package expression;

public class Sqrt implements Operand {
    private final Operand operand;

    public Sqrt(final Operand operand) {
        this.operand = operand;
    }

    public int calc(final int a) {
        if (a < 0) {
            throw new ArithmeticException("sqrt by negative integer");
        }
        return (int) Math.sqrt(a);
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
        return "sqrt(" + operand.toString() + ")";
    }

    @Override
    public boolean equals(Object obj) {
        if (obj != null && obj.getClass() == Sqrt.class) {
            return ((Sqrt) obj).operand.equals(this.operand);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return operand.hashCode() * 31;
    }
}