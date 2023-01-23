package _generated.parser_logical;

import java.util.regex.Pattern;

public enum TypeToken {
    END("\\$"),
	OR("\\|"),
	XOR("\\^"),
	AND("\\&"),
	INV("!"),
	NUM("(0|[1-9][0-9]*)"),
	OPEN("\\("),
	CLOSE("\\)");
    private final Pattern pattern;

    TypeToken (String regexp) {
        this.pattern = Pattern.compile(regexp);
    }

    public boolean match(String text) {
        return pattern.matcher(text).matches();
    }
}
