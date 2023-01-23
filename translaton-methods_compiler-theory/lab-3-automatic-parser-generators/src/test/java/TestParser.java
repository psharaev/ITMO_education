import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class TestParser {

    private static String parse(final String s) {
        final CharStream charStream = CharStreams.fromString(s);

        final MathLexer lexer = new MathLexer(charStream);
        final MathParser parser = new MathParser(new CommonTokenStream(lexer));

        return parser.translate().res.toString();
    }

    private static void assertEq(final String src, final String expected) {
        Assertions.assertEquals(expected, parse(src).trim());
    }

    @Test
    public void basic() {
        assertEq("a = 1;", "a = 1;");
        assertEq("a = 2 + 3;", "a = 5;");
        assertEq("a = 2 * 3;", "a = 6;");
        assertEq("a = 2 - 3;", "a = -1;");
        assertEq("a = 20 / 2;", "a = 10;");
    }

    @Test
    public void nested() {
        assertEq("a=(5);", "a = 5;");
        assertEq("a=(((5)));", "a = 5;");
        assertEq("a=(1 + ((5)));", "a = 6;");
        assertEq("a=(3 * ((5)));", "a = 15;");
        assertEq("a=(10 / ((5)));", "a = 2;");
        assertEq("a=(7 - ((5)));", "a = 2;");
        assertEq("a=2; b=3; c = 2 * 3 + 1;", "a = 2;\nb = 3;\nc = 7;");
    }

    @Test
    public void variables() {
        assertEq("a=2; b=3;", "a = 2;\nb = 3;");
        assertEq("a=2; b=a;", "a = 2;\nb = 2;");
        assertEq("a=2; b=3; c = a * b;", "a = 2;\nb = 3;\nc = 6;");
        assertEq("a=2; a=a;", "a = 2;\na = 2;");
    }

    @Test
    public void bigExpressions() {
        assertEq("a = 1 + 1 + 1 + 1 + 1;", "a = 5;");
        assertEq("a = 2 * 2 * 2 * 2 * 2;", "a = 32;");
        assertEq("a = 32 / 2 / 2 / 2 / 2;", "a = 2;");
        assertEq("a = 10 - 1 - 1 - 1 - 1;", "a = 6;");
        assertEq("a = 1 + 3 * 4 - 10 / 2;", "a = 8;");
        assertEq("x = 2 + 3 + 5 * 8 * 10 / 2 - 5 * 2;", "x = 195;");
    }

    @Test
    public void example() {
        assertEq("""
                a = 2;
                b = a + 2;
                c = a + b * (b - 3);
                a = 3;
                c = a + b * (b - 3);""", """
                a = 2;
                b = 4;
                c = 6;
                a = 3;
                c = 7;""");
    }
}
