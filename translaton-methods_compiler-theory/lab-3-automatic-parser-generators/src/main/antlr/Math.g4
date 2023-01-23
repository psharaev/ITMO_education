grammar Math;

@header {
    import java.util.*;
}

translate returns [StringBuilder res] @init {
    Map<String, Integer> vars = new HashMap<>();
    StringBuilder res = new StringBuilder();
    $res = res;
}
: file[vars, res]
;

file [Map<String, Integer> vars, StringBuilder res]
: (assign[vars, res])* EOF
;

assign [Map<String, Integer> vars, StringBuilder res]
: variable '=' expression[vars] ';' {
    Integer lastValue = $vars.put($variable.text, $expression.val);
    if (lastValue != null) {
        throw new IllegalStateException(String.format("var: \"%s\" is already assign with value: %d", $variable.text, lastValue));
    }
    $res.append(String.format("%s = %d;\n", $variable.text, $expression.val));
}
;

expression [Map<String, Integer> vars] returns [int val]
: addExpression[vars] { $val = $addExpression.val; }
;

addExpression [Map<String, Integer> vars] returns [int val]
: multiplyExpression[vars] addExpressionRest[vars, $multiplyExpression.val] { $val = $addExpressionRest.val; }
;

addExpressionRest [Map<String, Integer> vars, int acc] returns [int val]
: Add multiplyExpression[vars] { $acc = $acc + $multiplyExpression.val; } addExpressionRest[vars, $acc] { $val = $addExpressionRest.val; }
| Sub multiplyExpression[vars] { $acc = $acc - $multiplyExpression.val; } addExpressionRest[vars, $acc] { $val = $addExpressionRest.val; }
| { $val = $acc; }
;

multiplyExpression [Map<String, Integer> vars] returns [int val]
: leaf[vars] multiplyExpressionRest[vars, $leaf.val] { $val = $multiplyExpressionRest.val; }
;

multiplyExpressionRest [Map<String, Integer> vars, int acc] returns [int val]
: Mul leaf[vars] { $acc = $acc * $leaf.val; } multiplyExpressionRest[vars, $acc] { $val = $multiplyExpressionRest.val; }
| Div leaf[vars] { $acc = $acc / $leaf.val; } multiplyExpressionRest[vars, $acc] { $val = $multiplyExpressionRest.val; }
| { $val = $acc; }
;

leaf [Map<String, Integer> vars] returns [int val]
: number { $val = Integer.parseInt($number.text); }
| variable {
    Integer val = $vars.get($variable.text);
    if (val == null) {
        throw new IllegalStateException(String.format("var: \"%s\" is not assigned", $variable.text));
    }
    $val = val;
    }
| '(' expression[vars] ')' { $val = $expression.val; }
;

number
: DecimalNumber
;

variable
: Var
;

DecimalNumber: [0] | ([1-9] [0-9]*);
Var : [a-zA-Z] [a-zA-Z0-9]*;
WS : [ \n\t\r]+ -> skip;

Add: '+';
Sub: '-';
Mul: '*';
Div: '/';