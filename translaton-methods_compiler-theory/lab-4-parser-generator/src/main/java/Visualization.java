import _generated.parser_math.LexicalAnalyzer;
import _generated.parser_math.Parser;
import _generated.parser_math.Tree;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.stream.Collectors;

/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class Visualization {

    public static final DateTimeFormatter DATA_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
    private static int num = 1;

    public static void main(final String[] args) throws IOException {
        final BufferedReader scanner = new BufferedReader(new InputStreamReader(System.in));

        final String expression = scanner.lines().collect(Collectors.joining());

        final Parser.Expr tree = new Parser(new LexicalAnalyzer(expression)).expr();

        final StringBuilder sb = new StringBuilder();

        addln(sb, "digraph G {");
        addln(sb, "label=\"" + "Math expression: " + expression + ", val = " + tree.val + "\"");
        dfs(sb, tree);
        addln(sb, "}");


        Graphviz.fromString(sb.toString()).render(Format.PNG).toFile(
                new File(String.format("./images/%s.png", LocalDateTime.now().format(DATA_FORMATTER)))
        );
    }

    private static void addln(final StringBuilder sb, final String s) {
        sb.append(s);
        sb.append("\n");
    }

    private static void dfs(final StringBuilder sb, final Tree tree) {
        addln(sb, num + " [label=\"" + tree.getNode() + "\"]");
        final int cur = num;
        for (final var node : tree.getChildren()) {
            addln(sb, cur + "->" + ++num);
            dfs(sb, node);
        }
    }
}