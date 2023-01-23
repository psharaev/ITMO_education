import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;

/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class Parser {
    /*
    D  -> CD'
    D' -> |CD'
    D' -> ^CD'
    D' -> ε
    C  -> IC'
    C' -> &IC'
    С' -> ε
    I  -> !I
    I  -> F
    F  -> var
    F  -> (D)
     */
    private LexicalAnalyzer lex;


    private Tree D() throws ParseException {
        final Tree C = C();
        final Tree D_ = D_();
        return new Tree("D", C, D_);
    }

    private Tree D_() throws ParseException {
        switch (lex.curToken()) {
            case OR:
            case XOR: {
                final char symbol = lex.getSymbol();
                lex.nextToken();
                final Tree C = C();
                final Tree D_ = D_();
                return new Tree("D' " + symbol, C, D_);
            }
            default:
                return new Tree("D' ε");
        }
    }

    private Tree C() throws ParseException {
        final Tree I = I();
        final Tree C_ = C_();
        return new Tree("C", I, C_);
    }

    private Tree C_() throws ParseException {
        if (lex.curToken() == Token.AND) {
            final char symbol = lex.getSymbol();
            lex.nextToken();
            final Tree I = I();
            final Tree C_ = C_();
            return new Tree("C' " + symbol, I, C_);
        }
        return new Tree("C' ε");
    }

    private Tree I() throws ParseException {
        if (lex.curToken() == Token.INVERSE) {
            final char symbol = lex.getSymbol();
            lex.nextToken();
            final Tree I = I();
            return new Tree("I " + symbol, I);
        }
        final Tree F = F();
        return new Tree("I", F);
    }

    private Tree F() throws ParseException {
        switch (lex.curToken()) {
            case VARIABLE -> {
                final String name = lex.getString();
                lex.nextToken();
                return new Tree("F: " + name);
            }
            case LBRACKET -> {
                lex.nextToken();
                final Tree D = D();
                expect(Token.RBRACKET);
                return new Tree("F", D);
            }
            default -> throw error("Expected var or lbracket, actual: " + lex.curToken());
        }
    }

    private void expect(final Token token) throws ParseException {
        if (lex.curToken() == token) {
            lex.nextToken();
            return;
        }
        throw error("Expect: %s, actual: %s".formatted(token, lex.curToken()));
    }

    private ParseException error(final String msg) {
        return new ParseException(msg + ", pos: " + lex.getPos(), lex.getPos());
    }

    public Tree parse(final InputStream is) throws ParseException {
        lex = new LexicalAnalyzer(is);
        final Tree res = D();
        expect(Token.END);
        return res;
    }

    public Tree parse(final String s) throws ParseException {
        return parse(new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8)));
    }
}
