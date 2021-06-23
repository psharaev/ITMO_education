package expression;

public class LogicXOR extends BinaryExpression {

    public LogicXOR(final Operand left, final Operand right) {
        super("^", left, right);
    }

    @Override
    public int calc(int a, int b) {
        return a ^ b;
    }
}
