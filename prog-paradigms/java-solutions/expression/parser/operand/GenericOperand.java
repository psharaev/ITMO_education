package expression.parser.operand;

import expression.calculator.Calculator;

public interface GenericOperand<T> {
    T evaluate(final Calculator<T> calculator, final T x, final T y, final T z);
}
