import generators.FirstAndFollow;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.Set;

public class FirstAndFollowTest {
    private static final String MATH_GRAMMAR = """
            grammar math;

            expr () [int val] : term() exprS(term.val) {};

            exprS(int acc) [int val] :
            PLUS term() {$val = acc + term1.val;} exprS($val) {$val = exprS2.val;}
            | MINUS term() {$val = acc - term1.val;} exprS($val) {$val = exprS2.val;} | ε;

            term () [int val] : factor() termS(factor1.val) {$val = termS1.val;};

            termS (int acc) [int val] :
            MUL factor() {$val = acc * factor1.val;} termS($val) {$val = termS2.val;}
            | DIV factor() {$val = acc / factor1.val;} termS($val) {$val = termS2.val;} | ε;

            factor () [int val] :
            SIN factor() {$val = Math.sin(factor1.val);}
            | COS factor() {$val = Math.cos(factor1.val);}
            | NUM {$val = Integer.parseInt(token.text());}
            | OPEN expr() CLOSE {$val = expr1.val;}
            | MINUS NUM {$val = (-1) * Integer.parseInt(token.text());};
            """;

    private static final String PARENTHESES_GRAMMAR = """
            grammar parentheses;

            e () [] : OPEN e() CLOSE es() {} | ε;
            es () [] : e() es() {} | ε;

            """;

    private static FirstAndFollow firstAndFollowMath;
    private static FirstAndFollow firstAndFollowParentheses;

    @BeforeAll
    public static void init() {
        final CharStream charStreamMath = CharStreams.fromString(MATH_GRAMMAR);

        final MetaGrammarLexer lexerMath = new MetaGrammarLexer(charStreamMath);
        final MetaGrammarParser parserMath = new MetaGrammarParser(new CommonTokenStream(lexerMath));

        firstAndFollowMath = new FirstAndFollow(parserMath
                .metaGrammar()
                .grammar
        );

        final CharStream charStreamParentheses = CharStreams.fromString(PARENTHESES_GRAMMAR);

        final MetaGrammarLexer lexerParentheses = new MetaGrammarLexer(charStreamParentheses);
        final MetaGrammarParser parserParentheses = new MetaGrammarParser(new CommonTokenStream(lexerParentheses));

        firstAndFollowParentheses = new FirstAndFollow(parserParentheses
                .metaGrammar()
                .grammar
        );
    }

    @Test
    public void constructFirstMathTest() {
        final Map<String, Set<String>> first = firstAndFollowMath.getFirst();

        Assertions.assertEquals( 5, first.size());
        Assertions.assertEquals(Set.of("SIN", "COS", "OPEN", "NUM", "MINUS"), first.get("expr"));
        Assertions.assertEquals(Set.of("PLUS", "MINUS", "ε"), first.get("exprS"));
        Assertions.assertEquals(Set.of("SIN", "COS", "OPEN", "NUM", "MINUS"), first.get("term"));
        Assertions.assertEquals(Set.of("MUL", "DIV", "ε"), first.get("termS"));
        Assertions.assertEquals(Set.of("SIN", "COS", "OPEN", "NUM", "MINUS"), first.get("factor"));
    }

    @Test
    public void constructFollowMathTest() {
        final Map<String, Set<String>> follow = firstAndFollowMath.getFollow();

        Assertions.assertEquals( 5, follow.size());
        Assertions.assertEquals(Set.of("END", "CLOSE"), follow.get("expr"));
        Assertions.assertEquals(Set.of("END", "CLOSE"), follow.get("exprS"));
        Assertions.assertEquals(Set.of("PLUS", "MINUS", "END", "CLOSE"), follow.get("term"));
        Assertions.assertEquals(Set.of("PLUS", "MINUS", "END", "CLOSE"), follow.get("termS"));
        Assertions.assertEquals(Set.of("PLUS", "MINUS", "MUL", "DIV", "END", "CLOSE"), follow.get("factor"));
    }

    @Test
    public void constructFirstParenthesesTest() {
        final Map<String, Set<String>> first = firstAndFollowParentheses.getFirst();

        Assertions.assertEquals( 2, first.size());
        Assertions.assertEquals(Set.of("OPEN", "ε"), first.get("e"));
        Assertions.assertEquals(Set.of("OPEN", "ε"), first.get("es"));
    }

    @Test
    public void constructFollowParenthesesTest() {
        final Map<String, Set<String>> follow = firstAndFollowParentheses.getFollow();

        Assertions.assertEquals( 2, follow.size());
        Assertions.assertEquals(Set.of("END", "CLOSE", "OPEN"), follow.get("e"));
        Assertions.assertEquals(Set.of("END", "CLOSE", "OPEN"), follow.get("es"));
    }
}
