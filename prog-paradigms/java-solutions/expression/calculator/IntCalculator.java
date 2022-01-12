package expression.calculator;

public class IntCalculator implements Calculator<Integer> {

    @Override
    public Integer valueOf(int a) {
        return a;
    }

    @Override
    public Integer valueOf(String s) {
        return Integer.valueOf(s);
    }

    @Override
    public Integer add(Integer a, Integer b) {
        return a + b;
    }

    @Override
    public Integer subtract(Integer a, Integer b) {
        return a - b;
    }

    @Override
    public Integer multiply(Integer a, Integer b) {
        return a * b;
    }

    @Override
    public Integer divide(Integer a, Integer b) {
        return a / b;
    }

    @Override
    public Integer negate(Integer a) {
        return -a;
    }
}
