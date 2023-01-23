grammar MetaGrammar;

@header {
import java.util.*;
import meta.*;
}

metaGrammar returns[GrammarInfo grammar] @init {
    GrammarInfo grammar = new GrammarInfo();
    $grammar = grammar;
}
: grammarHeader rules[grammar] {
    $grammar.setNameGrammar($grammarHeader.nameGrammar);
  } EOF
;

grammarHeader returns[String nameGrammar]
: GRAMMAR name SEMICOLON { $nameGrammar = $name.text; }
;

rules[GrammarInfo grammar]
: grammarRule[grammar]*
;

grammarRule[GrammarInfo grammar]
: terminalRule[grammar]
| nonTerminalRule[grammar]
;

terminalRule[GrammarInfo grammar]
: name COLON regexp SEMICOLON { $grammar.addToken($name.text, $regexp.text); }
;

name
: NAME
| TOKEN_NAME
;

regexp
: REGEXP
;

nonTerminalRule[GrammarInfo grammar]
: name heritableAttr attr {RuleSignature ruleSignature = new RuleSignature($name.text, $attr.text, $heritableAttr.text);}
    COLON rule[grammar, ruleSignature]
    (OR rule[grammar, ruleSignature])* SEMICOLON
;

rule[GrammarInfo grammar, RuleSignature n] @init {
    List<Product> productions = new ArrayList<>();
}
: (product { productions.add($product.production); })+ { $grammar.putRuleSelector($n, new RuleSelector(productions)); }
| eps code? { $grammar.putRuleSelector($n, new RuleSelector(List.of(new Product(List.of(), $code.text)))); }
;

product returns [Product production] @init {
    List<RuleCalling> ruleCallings = new ArrayList<>();
}
: (name heritableAttr? { ruleCallings.add(new RuleCalling($name.text, $heritableAttr.text)); })+
    code { $production = new Product(ruleCallings, $code.text); }
;

eps: EPS;
code: CODE;
attr: EXPR_ATTR;
heritableAttr: HERIT_ATTR;

GRAMMAR: 'grammar';

TOKEN_NAME: [A-Z]+;
NAME: [a-z]+[A-Z]*;

EPS: 'ε';
COLON: ':';
OR: '|';
SEMICOLON: ';';
RETURNS: '->';
CODE: '{' .*? '}';
EXPR_ATTR: '[' .*? ']';
HERIT_ATTR: '(' .*? ')';
REGEXP: '"'.*?'"';

WHITESPACE: [ \t\r\n]+ -> skip;