package expression.calculator;

public class LongCalculator implements Calculator<Long> {

    @Override
    public Long valueOf(int a) {
        return (long) a;
    }

    @Override
    public Long valueOf(String s) {
        return Long.valueOf(s);
    }

    @Override
    public Long add(Long a, Long b) {
        return a + b;
    }

    @Override
    public Long subtract(Long a, Long b) {
        return a - b;
    }

    @Override
    public Long multiply(Long a, Long b) {
        return a * b;
    }

    @Override
    public Long divide(Long a, Long b) {
        return a / b;
    }

    @Override
    public Long negate(Long a) {
        return -a;
    }
}
