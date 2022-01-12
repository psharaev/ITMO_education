package expression.generic;

import expression.calculator.*;
import expression.parser.ParseException;
import expression.parser.operand.GenericOperand;
import expression.parser.ExpressionParser;

import java.util.Map;

public class GenericTabulator implements Tabulator {
    private final static Map<String, Calculator<?>> CALCULATORS = Map.of(
            "i", new CheckedIntCalculator(),
            "d", new DoubleCalculator(),
            "bi", new BigIntegerCalculator(),
            "u", new IntCalculator(),
            "l", new LongCalculator(),
            "s", new ShortCalculator()
    );

    @Override
    public Object[][][] tabulate(String mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) throws ParseException {
        return generate(CALCULATORS.get(mode), expression, x1, x2, y1, y2, z1, z2);
    }

    private <T> Object[][][] generate(final Calculator<T> calculator, String expression, int x1, int x2, int y1, int y2, int z1, int z2) throws ParseException {
        ExpressionParser<T> p = new ExpressionParser<>(calculator);
        GenericOperand<T> operand = p.parse(expression);

        final int lenX = x2 - x1 + 1,
                lenY = y2 - y1 + 1,
                lenZ = z2 - z1 + 1;

        Object[][][] res = new Object[lenX][lenY][lenZ];

        for (int x = 0; x < lenX; x++) {
            for (int y = 0; y < lenY; y++) {
                for (int z = 0; z < lenZ; z++) {
                    try {
                        res[x][y][z] = operand.evaluate(calculator, calculator.valueOf(x + x1),
                                calculator.valueOf(y + y1),
                                calculator.valueOf(z + z1));
                    } catch (ArithmeticException ignored) {

                    }
                }
            }
        }

        return res;
    }


    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("Enter calculate mode and expression");
            return;
        }

        GenericTabulator gt = new GenericTabulator();

        try {
            Object[][][] res = gt.tabulate(args[0].substring(1), args[1], -2, 2, -2, 2, -2, 2);
            dump(res, -2, 2, -2, 2, -2, 2);
        } catch (ParseException e) {
            System.err.println(e.toString());
        }
    }

    public static void dump(Object[][][] res, int x1, int x2, int y1, int y2, int z1, int z2) {
        final int lenX = x2 - x1 + 1,
                lenY = y2 - y1 + 1,
                lenZ = z2 - z1 + 1;

        for (int x = 0; x < lenX; x++) {

            System.out.println("X val: " + (x + x1));
            System.out.print(" Y\\Z ");
            for (int z = 0; z < lenZ; z++) {
                System.out.printf("%4s ", z + z1);
            }
            System.out.println();

            for (int y = 0; y < lenY; y++) {
                System.out.printf("%4s ", y + y1);
                for (int z = 0; z < lenZ; z++) {
                    System.out.format("%4s ", res[x][y][z]);
                }
                System.out.println();
            }

            System.out.println("*".repeat((lenY + 1) * 5));
            System.out.println();
        }
    }
}
