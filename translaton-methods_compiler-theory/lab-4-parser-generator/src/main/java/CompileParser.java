import generators.GeneratorLexicalAnalyzer;
import generators.GeneratorParser;
import meta.GrammarInfo;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class CompileParser {

    private static final Path PATH_TO_GRAMMAR_FILE = Path.of("./grammar.grammar");

    public static void main(final String[] args) throws IOException {
        final Path grammarFile;
        if (args == null || args.length == 0) {
            grammarFile = PATH_TO_GRAMMAR_FILE;
        } else if (args.length == 1 && args[0] != null) {
            grammarFile = Path.of(args[0]);
        } else {
            grammarFile = PATH_TO_GRAMMAR_FILE;
        }

        final String expressions = Files.readString(grammarFile);

        final CharStream charStream = CharStreams.fromString(expressions);

        final MetaGrammarLexer lexer = new MetaGrammarLexer(charStream);
        final MetaGrammarParser parser = new MetaGrammarParser(new CommonTokenStream(lexer));

        final GrammarInfo grammarInfo = parser.metaGrammar().grammar;
        System.out.println(grammarInfo.tokens().size());

        for (final var e : grammarInfo.tokens())
            System.out.println(e.name() + " " + e.regexp());

        System.out.println(grammarInfo.rules().size());

        for (final var e : grammarInfo.rules()) {
            System.out.println(e.signature() + " " + e.selector());
        }

        final GeneratorLexicalAnalyzer analyzer = new GeneratorLexicalAnalyzer(grammarInfo);
        analyzer.generate();
        final GeneratorParser generatorParser = new GeneratorParser(grammarInfo);
        generatorParser.generate();
    }
}

