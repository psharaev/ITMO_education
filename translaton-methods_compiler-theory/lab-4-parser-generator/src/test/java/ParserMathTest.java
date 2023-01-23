import _generated.parser_math.*;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class ParserMathTest {

    private static List<Arguments> correctExpression() {
        return Stream.of(
                        "-(123)*(123-123)",
                        "-3213*(73-41)+22",
                        "-(-13)",
                        "-0",
                        " 1",
                        "2 + 2 * 2",
                        "(1+2)*(-3*(7-4)+2)",
                        "2 / 2 + 2 * 2 - 4 * 4 + 1",
                        "( 2 +  2) / 41232 - 123 ",
                        "        123      ",
                        "      -228  + (34) ",
                        "123123",
                        "0",
                        "-12",
                        "-123 + -21312",
                        "-123/-123 + -123",
                        " -2",
                        "- 123",
                        "123 / 123 / 123 + 123"
                ).map(Arguments::of)
                .collect(Collectors.toList());
    }

    private static List<Arguments> errorExpressions() {
        return Stream.of(
                        "-",
                        "01",
                        "00",
                        "+",
                        "--12",
                        " - - 14",
                        "2 + + 2 * 2",
                        "//123",
                        "kek",
                        "* 213 * 123 ",
                        "        123   *   ",
                        "      -228 -  + (34) ",
                        "-123 + -21312 +  ",
                        "+-123/-123 + -123",
                        "( 1 + 2) sin",
                        " 123  ! 123 ",
                        " ( )",
                        " -( )",
                        " -( +)",
                        " -( * 4)",
                        " -(() * ())"
                ).map(Arguments::of)
                .collect(Collectors.toList());
    }

    private record Expr(String expr, int val) {
    }

    private static List<Arguments> checkValueExpression() {
        return Stream.of(
                        new Expr("4 / 2 / 2", 1),
                        new Expr("64 / 2 / 2 / 2 / 2 / 2 / 2", 1),
                        new Expr("64 / 2 / 2 / 2 / 2 / 2  - 2", 0),
                        new Expr("64 / 2  * 2", 64),
                        new Expr("2 + 2 * 2", 6),
                        new Expr("-(2 + 2) * 2 - 8", -16),
                        new Expr("9 - 1 - 2 - 3 ", 3),
                        new Expr("1 + 2 + 3 * 3 - (12 + 12) * 3 + 72", 12),
                        new Expr("1 * 2 * 3 * (4 + 1 - 1) - 21", 3),
                        new Expr("- 2 - 2 - 2", -6),
                        new Expr("-2 + 2 + 1", 1),
                        new Expr("1", 1),
                        new Expr("25 - 4 * 8 / 2 - 4 + 8 / 2", 9),
                        new Expr("12 - 2 * (5 - 4 / 2)", 6),
                        new Expr("300 - 29 * 4", 184),
                        new Expr("80 - (46 - 14)", 48),
                        new Expr("9 / 3 * 2", 6),
                        new Expr("15 - (7 + 3) / 2", 10),
                        new Expr("3 * (1 + 3) + 8", 20)
                ).map(Arguments::of)
                .collect(Collectors.toList());
    }

    @ParameterizedTest(name = "Test {0}")
    @MethodSource("correctExpression")
    public void testCorrectExpression(final String expr) {
        Assertions.assertDoesNotThrow(() -> new Parser(new LexicalAnalyzer(expr)).expr());
    }

    @ParameterizedTest(name = "Test {0}")
    @MethodSource("errorExpressions")
    public void testErrorExpressions(final String expr) {
        Assertions.assertThrows(ParseException.class,
                () -> new Parser(new LexicalAnalyzer(expr)).expr());
    }

    @ParameterizedTest(name = "Test{0}")
    @MethodSource("checkValueExpression")
    public void checkValueExpressionTest(final Expr expr) {
        Assertions.assertEquals(expr.val, new Parser(new LexicalAnalyzer(expr.expr)).expr().val);
    }
}
