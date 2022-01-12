package expression.calculator;

public class DoubleCalculator implements Calculator<Double> {

    @Override
    public Double valueOf(int a) {
        return (double) a;
    }

    @Override
    public Double valueOf(String s) {
        return Double.valueOf(s);
    }

    @Override
    public Double add(Double a, Double b) {
        return a + b;
    }

    @Override
    public Double subtract(Double a, Double b) {
        return a - b;
    }

    @Override
    public Double multiply(Double a, Double b) {
        return a * b;
    }

    @Override
    public Double divide(Double a, Double b) {
        return a / b;
    }

    @Override
    public Double negate(Double a) {
        return -a;
    }
}
