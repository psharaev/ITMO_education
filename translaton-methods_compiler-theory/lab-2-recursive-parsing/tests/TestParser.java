import org.junit.Assert;
import org.junit.Test;

import java.text.ParseException;
import java.util.Random;

/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class TestParser {
    private static final Parser parser = new Parser();
    private static final Random random = new Random();
    private static final char[] BINARY_OPERATIONS = new char[]{'|', '&', '^'};
    private static final char[] VARIABLES;
    public static final int COUNT_MULTIPLY_TESTS = 3;
    public static final int COUNT_VARS_TEST = 5;
    public static final int COUNT_OPERATIONS_TEST = 3;

    static {
        final int countVars = 'z' - 'a' + 1;
        VARIABLES = new char[countVars];
        for (int i = 0; i < countVars; i++) {
            VARIABLES[i] = (char) ('a' + i);
        }
    }


    @Test
    public void basicAccept() {
        accept("#");
        accept("(#)");
        accept("((#))");
        accept("(((#)))");
        accept("!#");
        accept("!!#");
        accept("!!!#");
        accept("#@#");
    }

    @Test
    public void juniorAccept() {
        accept("(#@#)");
        accept("((#@#))");
        accept("(!(#@#))");
        accept("((#@#)@#)");
        accept("((!#@!#)@!#)");
        accept("(#@(#@#))");
        accept("(!#@(!#@!#))");
        accept("!(!(#@#)@#)@#");
        accept("!!!!!!!!#@#@!!!!!!!!#");
    }

    @Test
    public void basicReject() {
        reject("!");
        reject("(");
        reject(")");
        reject("#(");
        reject("#)");
        reject("(()");
        reject("())");
        reject("()");
        reject(")(");
        reject("!!!");
        reject("@");
        reject("@@@");
        reject("#@");
        reject("!#@");
        reject("!(#@)");
        reject("@#");
        reject("@!#");
    }

    @Test
    public void juniorReject() {
        reject("(((((((((((((");
        reject("(((#(((@@(((((((");
        reject(")))))))))))");
        reject("#!@#");
        reject("!!!!!!!!#@#!!!!!!!!#");
    }


    private static void accept(final String s) {
        accept(s, 5);
    }

    private static void accept(final String s, final int coefficient) {
        multiplyTest(s, true, coefficient * COUNT_MULTIPLY_TESTS);
    }

    private static void reject(final String s) {
        reject(s, 5);
    }

    private static void reject(final String s, final int coefficient) {
        multiplyTest(s, false, coefficient * COUNT_MULTIPLY_TESTS);
    }

    private static void multiplyTest(final String s, final boolean isAcceptance, final int countTests) {
        for (int j = 0; j < COUNT_OPERATIONS_TEST; j++) {
            final String replacedOperations = replaceOperations(s);
            for (int k = 0; k < COUNT_VARS_TEST; k++) {
                final String replacedVars = replaceVars(replacedOperations);
                for (int i = 0; i < countTests; i++) {
                    checkRandomWhitespaces(replacedVars, isAcceptance);
                }
            }
        }
    }

    private static String replaceOperations(final String s) {
        final int countOperations = countChar(s, '@');
        String res = s;
        for (int i = 0; i < countOperations; i++) {
            res = res.replaceFirst("@", String.valueOf(BINARY_OPERATIONS[random.nextInt(BINARY_OPERATIONS.length)]));
        }
        return res;
    }

    private static String replaceVars(final String s) {
        final int countVars = countChar(s, '#');
        String res = s;
        for (int i = 0; i < countVars; i++) {
            res = res.replaceFirst("#", String.valueOf(VARIABLES[random.nextInt(VARIABLES.length)]));
        }
        return res;
    }

    private static int countChar(final String s, final char ch) {
        int res = 0;
        for (final char c : s.toCharArray()) {
            if (c == ch) {
                res++;
            }
        }
        return res;
    }


    private static void checkRandomWhitespaces(final String s, final boolean isAcceptance) {
        final int countNeedSpaces = s.length() + 1;
        for (int countSpaces = 0; countSpaces <= countNeedSpaces; countSpaces++) {
            final StringBuilder sb = new StringBuilder(s);
            if (countSpaces == 0) {
                System.out.println("*".repeat(80));
            }
            for (int i = 0; i < countSpaces; i++) {
                sb.insert(random.nextInt(sb.length() + 1), getRandomWhitespace());
            }
            if (isAcceptance) {
                assertAcceptance(sb.toString());
            } else {
                assertReject(sb.toString());
            }
        }
    }

    private static char getRandomWhitespace() {
        final int number = random.nextInt(4);
        return switch (number) {
            case 0 -> ' ';
            case 1 -> '\r';
            case 2 -> '\t';
            default -> '\n';
        };
    }

    private static String expressionWithPrefix(final String s) {
        return "expression: \"%s\"".formatted(s);
    }

    private static void assertAcceptance(final String s) {
        System.out.println(expressionWithPrefix(s));
        try {
            Assert.assertNotNull(parser.parse(s));
        } catch (final ParseException e) {
            Assert.fail(e.getMessage());
        }
    }

    private static void assertReject(final String s) {
        System.out.println(expressionWithPrefix(s));
        try {
            parser.parse(s);
            Assert.fail();
        } catch (final ParseException e) {
            Assert.assertNotNull(e);
        }

    }
}
