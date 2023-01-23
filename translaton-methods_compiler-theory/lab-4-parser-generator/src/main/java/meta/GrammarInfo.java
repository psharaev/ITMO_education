package meta;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class GrammarInfo {
    private String nameGrammar;

    private final List<TerminalToken> tokens = new ArrayList<>();
    private final List<RuleAndSelector> rules = new ArrayList<>();

    public List<TerminalToken> tokens() {
        return tokens;
    }

    public List<RuleAndSelector> rules() {
        return rules;
    }

    public void setNameGrammar(final String nameGrammar) {
        this.nameGrammar = nameGrammar;
    }

    public String getNameGrammar() {
        return nameGrammar;
    }

    public void addToken(final String name, final String regexp) {
        tokens.add(new TerminalToken(name, regexp));
    }

    public void putRuleSelector(final RuleSignature nonTerminal, final RuleSelector rule) {
        rules.add(new RuleAndSelector(nonTerminal, rule));
    }
}
