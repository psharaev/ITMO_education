package expression;

public class Subtract extends BinaryExpression {

    public Subtract(final Operand left, final Operand right) {
        super("-", left, right);
    }

    @Override
    public int calc(final int a, final int b) {
        return a - b;
    }
}
