package expression;

public class LogicAND extends BinaryExpression {

    public LogicAND(Operand left, Operand right) {
        super("&", left, right);
    }

    @Override
    public int calc(int a, int b) {
        return a & b;
    }
}
