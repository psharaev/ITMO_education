package expression;

public class Add extends BinaryExpression {

    public Add(final Operand left, final Operand right) {
        super("+", left, right);
    }

    @Override
    public int calc(final int a, final int b) {
        return a + b;
    }
}
