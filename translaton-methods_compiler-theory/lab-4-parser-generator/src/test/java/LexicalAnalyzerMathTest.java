import _generated.parser_math.LexicalAnalyzer;
import _generated.parser_math.ParseException;
import _generated.parser_math.TypeToken;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.List;


public class LexicalAnalyzerMathTest {

    private static final List<TypeToken> tokens = List.of(
            TypeToken.NUM,
            TypeToken.PLUS,
            TypeToken.MINUS,
            TypeToken.DIV,
            TypeToken.MUL,
            TypeToken.END
    );

    @Test
    public void testTokensWithoutWhitespace() {
        checkResult(new LexicalAnalyzer("123123+-/*"));
    }

    @Test
    public void testTokensWithWhitespace() {
        checkResult(new LexicalAnalyzer("   3  + - \t / * "));
    }

    @Test
    public void testTokensWithError() {
        final RuntimeException thrown = Assertions.assertThrows(ParseException.class,
                () -> checkResult(new LexicalAnalyzer("123123  + - kkk / *  ! ")));

        Assertions.assertEquals(thrown.getMessage(), "No valid token on pos: " + 12);
    }

    private void checkResult(final LexicalAnalyzer analyzer) {
        analyzer.nextToken();
        for (final var token : tokens) {
            Assertions.assertEquals(analyzer.getToken().typeToken(), token);
            analyzer.nextToken();
        }
    }
}