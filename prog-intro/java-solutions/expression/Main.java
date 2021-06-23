package expression;

public class Main {
    public static void main(String[] args) {
        int x = Integer.parseInt(args[0]);
        // 7
        // ((2 * x) - 3)
        // true
        // false
        System.out.println(new Subtract(
                new Multiply(
                        new Const(2),
                        new Variable("x")
                ),
                new Const(3)
        ).evaluate(5));
        System.out.println(new Subtract(
                new Multiply(
                        new Const(2),
                        new Variable("x")
                ),
                new Const(3)
        ).toString());
        System.out.println(new Multiply(new Const(2), new Variable("x"))
                .equals(new Multiply(new Const(2), new Variable("x"))));
        System.out.println(new Multiply(new Const(2), new Variable("x"))
                .equals(new Multiply(new Variable("x"), new Const(2))));
        System.out.println(
                new Add(
                        new Subtract(
                                new Multiply(
                                        new Variable("x"),
                                        new Variable("x")
                                ),
                                new Multiply(
                                        new Const(2),
                                        new Variable("x")
                                )
                        ),
                        new Const(1)
                ).evaluate(x)
        );
    }
}
