package expression;

public class LogicOR extends BinaryExpression {

    public LogicOR(final Operand left, final Operand right) {
        super("|", left, right);
    }

    @Override
    public int calc(int a, int b) {
        return a | b;
    }
}
