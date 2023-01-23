import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;

import java.io.IOException;

/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class Main {
    public static void main(final String[] args) throws IOException {
        final CharStream charStream = CharStreams.fromStream(System.in);

        final MathLexer lexer = new MathLexer(charStream);
        final MathParser parser = new MathParser(new CommonTokenStream(lexer));

        try {
            System.out.println(parser.translate().res);
        } catch (final IllegalStateException e) {
            System.err.println(e.getMessage());
        }
    }
}