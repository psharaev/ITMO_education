package _generated.parser_math;

public class Parser {
    private final LexicalAnalyzer tokens;
    private Token token;

    private void nextToken() {
        tokens.nextToken();
        token = tokens.getToken();
    }

    public Parser(LexicalAnalyzer tokens) {
        this.tokens = tokens;
        nextToken();
    }

    public ExprS exprS(int acc) {
        ExprS res = new ExprS("exprS");
        switch(token.typeToken()) {
            case PLUS -> {
                if (token.typeToken() != TypeToken.PLUS) {
                    throw new ParseException("No valid token: " + token.text());
                }
                String PLUS0 = token.text();
                res.addChild(token.text());
                nextToken();
                Term term1 = term();
                res.addChild(term1);
                res.val = acc + term1.val;
                ExprS exprS2 = exprS(res.val);
                res.addChild(exprS2);
                res.val = exprS2.val;
            }
            case MINUS -> {
                if (token.typeToken() != TypeToken.MINUS) {
                    throw new ParseException("No valid token: " + token.text());
                }
                String MINUS0 = token.text();
                res.addChild(token.text());
                nextToken();
                Term term1 = term();
                res.addChild(term1);
                res.val = acc - term1.val;
                ExprS exprS2 = exprS(res.val);
                res.addChild(exprS2);
                res.val = exprS2.val;
            }
            case END, CLOSE -> {
                res.addChild("eps");
                res.val = acc;
            }
            default ->
                throw new ParseException("No valid token: " + token.text());
        }

        return res;
    }

    public TermS termS(int acc) {
        TermS res = new TermS("termS");
        switch(token.typeToken()) {
            case MUL -> {
                if (token.typeToken() != TypeToken.MUL) {
                    throw new ParseException("No valid token: " + token.text());
                }
                String MUL0 = token.text();
                res.addChild(token.text());
                nextToken();
                Factor factor1 = factor();
                res.addChild(factor1);
                res.val = acc * factor1.val;
                TermS termS2 = termS(res.val);
                res.addChild(termS2);
                res.val = termS2.val;
            }
            case DIV -> {
                if (token.typeToken() != TypeToken.DIV) {
                    throw new ParseException("No valid token: " + token.text());
                }
                String DIV0 = token.text();
                res.addChild(token.text());
                nextToken();
                Factor factor1 = factor();
                res.addChild(factor1);
                res.val = acc / factor1.val;
                TermS termS2 = termS(res.val);
                res.addChild(termS2);
                res.val = termS2.val;
            }
            case END, CLOSE, PLUS, MINUS -> {
                res.addChild("eps");
                res.val = acc;
            }
            default ->
                throw new ParseException("No valid token: " + token.text());
        }

        return res;
    }

    public Term term() {
        Term res = new Term("term");
        switch(token.typeToken()) {
            case NUM, OPEN, MINUS -> {
                Factor factor0 = factor();
                res.addChild(factor0);
                TermS termS1 = termS(factor0.val);
                res.addChild(termS1);
                res.val = termS1.val;
            }
            default ->
                throw new ParseException("No valid token: " + token.text());
        }

        return res;
    }

    public Unary unary() {
        Unary res = new Unary("unary");
        switch(token.typeToken()) {
            case NUM -> {
                if (token.typeToken() != TypeToken.NUM) {
                    throw new ParseException("No valid token: " + token.text());
                }
                String NUM0 = token.text();
                res.addChild(token.text());
                nextToken();
                res.val = Integer.parseInt("-" + NUM0);
            }
            case OPEN -> {
                if (token.typeToken() != TypeToken.OPEN) {
                    throw new ParseException("No valid token: " + token.text());
                }
                String OPEN0 = token.text();
                res.addChild(token.text());
                nextToken();
                Expr expr1 = expr();
                res.addChild(expr1);
                if (token.typeToken() != TypeToken.CLOSE) {
                    throw new ParseException("No valid token: " + token.text());
                }
                String CLOSE2 = token.text();
                res.addChild(token.text());
                nextToken();
                res.val = -(expr1.val);
            }
            default ->
                throw new ParseException("No valid token: " + token.text());
        }

        return res;
    }

    public Factor factor() {
        Factor res = new Factor("factor");
        switch(token.typeToken()) {
            case NUM -> {
                if (token.typeToken() != TypeToken.NUM) {
                    throw new ParseException("No valid token: " + token.text());
                }
                String NUM0 = token.text();
                res.addChild(token.text());
                nextToken();
                res.val = Integer.parseInt(NUM0);
            }
            case OPEN -> {
                if (token.typeToken() != TypeToken.OPEN) {
                    throw new ParseException("No valid token: " + token.text());
                }
                String OPEN0 = token.text();
                res.addChild(token.text());
                nextToken();
                Expr expr1 = expr();
                res.addChild(expr1);
                if (token.typeToken() != TypeToken.CLOSE) {
                    throw new ParseException("No valid token: " + token.text());
                }
                String CLOSE2 = token.text();
                res.addChild(token.text());
                nextToken();
                res.val = expr1.val;
            }
            case MINUS -> {
                if (token.typeToken() != TypeToken.MINUS) {
                    throw new ParseException("No valid token: " + token.text());
                }
                String MINUS0 = token.text();
                res.addChild(token.text());
                nextToken();
                Unary unary1 = unary();
                res.addChild(unary1);
                res.val = unary1.val;
            }
            default ->
                throw new ParseException("No valid token: " + token.text());
        }

        return res;
    }

    public Expr expr() {
        Expr res = new Expr("expr");
        switch(token.typeToken()) {
            case NUM, OPEN, MINUS -> {
                Term term0 = term();
                res.addChild(term0);
                ExprS exprS1 = exprS(term0.val);
                res.addChild(exprS1);
                res.val = exprS1.val;
            }
            default ->
                throw new ParseException("No valid token: " + token.text());
        }

        return res;
    }


    public static class ExprS extends Tree {
        public int val;
        public ExprS(String node) {
            super(node);
        }
    }

    public static class TermS extends Tree {
        public int val;
        public TermS(String node) {
            super(node);
        }
    }

    public static class Term extends Tree {
        public int val;
        public Term(String node) {
            super(node);
        }
    }

    public static class Unary extends Tree {
        public int val;
        public Unary(String node) {
            super(node);
        }
    }

    public static class Factor extends Tree {
        public int val;
        public Factor(String node) {
            super(node);
        }
    }

    public static class Expr extends Tree {
        public int val;
        public Expr(String node) {
            super(node);
        }
    }

}
