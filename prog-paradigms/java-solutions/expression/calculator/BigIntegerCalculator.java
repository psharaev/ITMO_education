package expression.calculator;

import java.math.BigInteger;

public class BigIntegerCalculator implements Calculator<BigInteger> {

    @Override
    public BigInteger valueOf(int a) {
        return BigInteger.valueOf(a);
    }

    @Override
    public BigInteger valueOf(String s) {
        return new BigInteger(s);
    }

    @Override
    public BigInteger add(BigInteger a, BigInteger b) {
        return a.add(b);
    }

    @Override
    public BigInteger subtract(BigInteger a, BigInteger b) {
        return a.subtract(b);
    }

    @Override
    public BigInteger multiply(BigInteger a, BigInteger b) {
        return a.multiply(b);
    }

    @Override
    public BigInteger divide(BigInteger a, BigInteger b) {
        return a.divide(b);
    }

    @Override
    public BigInteger negate(BigInteger a) {
        return a.negate();
    }
}
