package expression.calculator;

public class CheckedIntCalculator implements Calculator<Integer> {

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
        int r = a + b;
        if (((a ^ r) & (b ^ r)) < 0) {
            throw new ArithmeticException("integer overflow");
        }
        return r;
    }

    @Override
    public Integer subtract(Integer a, Integer b) {
        int r = a - b;
        if (((a ^ b) & (a ^ r)) < 0) {
            throw new ArithmeticException("integer overflow");
        }
        return r;
    }

    @Override
    public Integer multiply(Integer a, Integer b) {
        int r = a * b;
        if (((b != 0) && (r / b != a)) || (a == Integer.MIN_VALUE && b == -1)) {
            throw new ArithmeticException("int overflow");
        }
        return r;
    }

    @Override
    public Integer divide(Integer a, Integer b) {
        if (b == 0) {
            throw new ArithmeticException("division by zero");
        }
        if (a == Integer.MIN_VALUE && b == -1) {
            throw new ArithmeticException("int overflow");
        }
        return a / b;
    }

    @Override
    public Integer negate(Integer a) {
        if (a == Integer.MIN_VALUE) {
            throw new ArithmeticException("overflow");
        }
        return -a;
    }
}
