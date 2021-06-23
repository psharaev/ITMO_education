package expression;

public class Const implements Operand {

    private final int constant;

    public Const(final int constant) {
        this.constant = constant;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj != null && obj.getClass() == Const.class) {
            return ((Const) obj).constant == this.constant;
        }
        return false;
    }

    public int evaluate(int num) {
        return constant;
    }

    public int evaluate(int x, int y, int z) {
        return constant;
    }

    @Override
    public String toString() {
        return Integer.toString(constant);
    }

    @Override
    public int hashCode() {
        return constant;
    }
}
