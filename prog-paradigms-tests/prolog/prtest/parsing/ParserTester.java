package prtest.parsing;

import base.TestCounter;
import jstest.Engine;
import jstest.expression.BaseTester;
import jstest.expression.Builder;
import jstest.expression.Dialect;
import jstest.expression.Language;
import prtest.PrologEngine;
import prtest.Rule;
import prtest.Value;

import java.nio.file.Path;
import java.util.function.Function;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ParserTester extends BaseTester<Value, PrologEngine> {
    private static final Function<String, String> PREFIXER = name -> Character.isLetter(name.charAt(0)) ? "op_" + name : name;

    public ParserTester(final TestCounter counter, final Language language, final String parse) {
        super(
                counter,
                new PrologEngine(
                        Path.of("expression.pl"),
                        Rule.func("evaluate", 2),
                        new Rule(parse, 2)
                ),
                language,
                true
        );
        BaseTester.TESTS = 44;
    }

    @Override
    protected void test(final Engine.Result<Value> prepared, final String unparsed) {
        counter.test(() -> engine.toString(prepared).assertEquals(unparsed));
    }

    public static final Dialect PARSED = new Dialect("variable(%s)", "const(%s.0)", "operation({op}, {args})", ", ")
            .renamer(PREFIXER.andThen(String::toLowerCase));

    /* package-private*/ static base.Selector.Composite<Builder> builder() {
        return Builder.selector(
                ParserTest.class,
                mode -> false,
                (builder, counter) -> {
                    final Mode mode = counter.mode() == 1 ? Mode.INFIX : Mode.SUFFIX;
                    return new ParserTester(counter, builder.aliased(PARSED, mode.unparsed), mode.parse);
                },
                "easy", "hard"
        );
    }

    private enum Mode {
        SUFFIX("suffix_str", new Dialect("%s", "%s.0", "({args} {op})", " " )),
        INFIX("infix_str", new Dialect(
                "%s",
                "%s.0",
                (op, args) -> {
                    switch (args.size()) {
                        case 1: return op + "(" + args.get(0) + ")";
                        case 2: return "(" + args.get(0) + " " + op + " " + args.get(1) + ")";
                        default: throw new AssertionError("Unsupported op " + op + "/" + args.size());
                    }
                }
        ));

        private final Dialect unparsed;
        private final String parse;

        Mode(final String parse, final Dialect unparsed) {
            this.unparsed = unparsed;
            this.parse = parse;
        }
    }
}
