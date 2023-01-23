package generators;

import error.GenerateException;
import meta.GrammarInfo;
import meta.TerminalToken;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Collectors;

/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class GeneratorLexicalAnalyzer {
    private static final String TAB = "\t";

    private final GrammarInfo grammarInfo;
    private final Path path;

    public GeneratorLexicalAnalyzer(final GrammarInfo grammarInfo) {
        this(grammarInfo, Path.of("./src/main/java/_generated/parser_" + grammarInfo.getNameGrammar()));
    }

    public GeneratorLexicalAnalyzer(final GrammarInfo grammarInfo, final Path path) {
        this.grammarInfo = grammarInfo;
        this.path = path;
    }

    public void generate() {
        try {
            Files.createDirectories(path);
        } catch (final IOException ignored) {
        }

        generateTokenClass();
        generateTypeToken();
        generateLexicalAnalyzer();
    }

    private void generateTokenClass() {
        final String sourceCodeToken = String.format("""
                package _generated.parser_%s;

                public record Token (TypeToken typeToken, String text) {
                    @Override
                    public String toString() {
                        return typeToken.name();
                    }
                }""", grammarInfo.getNameGrammar());

        try (final BufferedWriter bufferedWriter = Files.newBufferedWriter(Path.of(path + "/Token.java"))) {
            bufferedWriter.write(sourceCodeToken);
        } catch (final IOException e) {
            throw new GenerateException("Can't create Token.java.");
        }
    }

    private void generateTypeToken() {
        final String startClass = String.format("""
                package _generated.parser_%s;
                                
                import java.util.regex.Pattern;
                                
                public enum TypeToken {
                    END("\\\\$"),""", grammarInfo.getNameGrammar());

        final String endClass = """
                    private final Pattern pattern;
                                
                    TypeToken (String regexp) {
                        this.pattern = Pattern.compile(regexp);
                    }
                                
                    public boolean match(String text) {
                        return pattern.matcher(text).matches();
                    }
                }
                """;
        try (final BufferedWriter bufferedWriter = Files.newBufferedWriter(Path.of(path + "/TypeToken.java"))) {
            bufferedWriter.write(startClass
                    + grammarInfo
                    .tokens()
                    .stream()
                    .map(entry -> TAB + entry.name() + "(" + entry.regexp() + ")")
                    .collect(Collectors.joining(",\n", "\n", ";\n"))
                    + endClass);
        } catch (final IOException e) {
            throw new GenerateException("Can't create TypeToken.java");
        }
    }

    private void generateLexicalAnalyzer() {
        final String startClass = String.format("""
                        package _generated.parser_%s;
                               
                        import java.util.regex.Matcher;
                        import java.util.regex.Pattern;
                                        
                        public class LexicalAnalyzer {
                                        
                            private final static Pattern PATTERN_EXPRESSION = Pattern.compile(""",
                grammarInfo.getNameGrammar());

        final String endClass = """
                );
                                
                    private final Matcher tokenMatcher;
                                
                    private Token curToken;
                                
                    public LexicalAnalyzer(String expression) {
                        this.tokenMatcher = PATTERN_EXPRESSION.matcher(expression);
                    }
                                
                    public void nextToken() {
                        while (tokenMatcher.find()) {
                                
                            if (Character.isWhitespace(tokenMatcher
                                    .group()
                                    .charAt(0))
                            ) {
                                continue;
                            }
                                
                            for (var typeToken : TypeToken.values()) {
                                String tokenStr = tokenMatcher.group();
                                if (typeToken.match(tokenStr)) {
                                    curToken = new Token(typeToken, tokenStr);
                                    return;
                                }
                            }
                                
                            throw new ParseException("No valid token on pos: " + tokenMatcher.start());
                        }
                                
                        curToken = new Token(TypeToken.END, "$");
                    }
                                
                    public Token getToken() {
                        return curToken;
                    }
                }
                """;
        try (final BufferedWriter bufferedWriter = Files.newBufferedWriter(Path.of(path + "/LexicalAnalyzer.java"))) {
            bufferedWriter.write(startClass
                    + grammarInfo
                    .tokens()
                    .stream()
                    .map(TerminalToken::regexp)
                    .map(s -> s.substring(1, s.length() - 1))
                    .collect(Collectors.joining("|", "\"", "|.\""))
                    + endClass);
        } catch (final IOException e) {
            throw new GenerateException("Can't create TypeToken.java");
        }
    }
}
