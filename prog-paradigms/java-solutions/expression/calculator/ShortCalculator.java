package expression.calculator;

public class ShortCalculator implements Calculator<Short> {

    @Override
    public Short valueOf(int a) {
        return (short) a;
    }

    @Override
    public Short valueOf(String s) {
        return Short.valueOf(s);
    }

    @Override
    public Short add(Short a, Short b) {
        return (short) (a + b);
    }

    @Override
    public Short subtract(Short a, Short b) {
        return (short) (a - b);
    }

    @Override
    public Short multiply(Short a, Short b) {
        return (short) (a * b);
    }

    @Override
    public Short divide(Short a, Short b) {
        return (short) (a / b);
    }

    @Override
    public Short negate(Short a) {
        return (short) -a;
    }
}
