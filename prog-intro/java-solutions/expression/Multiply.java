package expression;

public class Multiply extends BinaryExpression {

    public Multiply(final Operand left, final Operand right) {
        super("*", left, right);
    }

    @Override
    public int calc(final int a, final int b) {
        return a * b;
    }
}
