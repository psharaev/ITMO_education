package expression.calculator;

public interface Calculator<T> {
    T valueOf(final int a);

    T valueOf(final String s);

    T add(final T a, final T b);

    T subtract(final T a, final T b);

    T multiply(final T a, final T b);

    T divide(final T a, final T b);

    T negate(final T a);
}
