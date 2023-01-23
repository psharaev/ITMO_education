import java.io.IOException;
import java.io.InputStream;
import java.text.ParseException;

/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class LexicalAnalyzer {
    private final InputStream is;
    private int curChar;
    private String curString;
    private int countRead;
    private Token curToken;

    public LexicalAnalyzer(final InputStream is) throws ParseException {
        this.is = is;
        countRead = 0;
        nextChar();
        nextToken();
    }

    private void nextChar() throws ParseException {
        countRead++;
        try {
            curChar = is.read();
        } catch (final IOException e) {
            throw new ParseException(e.getMessage(), countRead);
        }
    }

    public void nextToken() throws ParseException {
        while (curChar != -1 && Character.isWhitespace((char) curChar)) {
            nextChar();
        }
        if (curChar == -1 || curChar == '$') {
            curToken = Token.END;
            curChar = '$';
            return;
        }

        if (Character.isLetter((char) curChar)) {
            curToken = Token.VARIABLE;
            curString = nextVar();
            return;
        }

        switch (curChar) {
            case '(' -> curToken = Token.LBRACKET;
            case ')' -> curToken = Token.RBRACKET;
            case '|' -> curToken = Token.OR;
            case '&' -> curToken = Token.AND;
            case '^' -> curToken = Token.XOR;
            case '!' -> curToken = Token.INVERSE;
            default -> throw new ParseException("Illegal character" + (char) curChar, countRead);
        }
        nextChar();
    }

    private String nextVar() throws ParseException {
        final StringBuilder sb = new StringBuilder();
        do {
            sb.append((char) curChar);
            nextChar();
        } while (curChar != -1 && Character.isLetter((char) curChar));
        return sb.toString();
    }

    public String getString() {
        return curString;
    }

    public Token curToken() {
        return curToken;
    }

    public int getPos() {
        return countRead;
    }

    public char getSymbol() {
        return (char) curChar;
    }
}