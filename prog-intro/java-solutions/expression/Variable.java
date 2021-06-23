package expression;

public class Variable implements Operand {

    private final String var;

    public Variable(final String var) {
        this.var = var;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj != null && obj.getClass() == Variable.class) {
            return ((Variable) obj).var.equals(this.var);
        }
        return false;
    }

    public int evaluate(int num) {
        return num;
    }

    public int evaluate(int x, int y, int z) {
        switch (var) {
            case "x":
                return x;
            case "y":
                return y;
            case "z":
                return z;
            default:
                return 0;
        }
    }

    @Override
    public String toString() {
        return var;
    }

    @Override
    public int hashCode() {
        return var.hashCode();
    }
}
