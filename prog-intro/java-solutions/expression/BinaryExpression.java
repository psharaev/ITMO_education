package expression;

public abstract class BinaryExpression implements Operand {

    private final String operator;

    private final Operand left;
    private final Operand right;

    public BinaryExpression(final String operator, final Operand left, final Operand right) {
        this.operator = operator;
        this.left = left;
        this.right = right;
    }

    public abstract int calc(final int a, final int b);

    @Override
    public int evaluate(int num) {
        return calc(left.evaluate(num), right.evaluate(num));
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return calc(left.evaluate(x, y, z), right.evaluate(x, y, z));
    }

    @Override
    public String toString() {
        return String.format("(%s %s %s)", left.toString(), operator, right.toString());
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof BinaryExpression) {
            BinaryExpression that = (BinaryExpression) obj;
            return this.getClass() == that.getClass() && left.equals(that.left) && right.equals(that.right);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return (31 * left.hashCode() + right.hashCode()) * 31 + getClass().hashCode();
    }
}