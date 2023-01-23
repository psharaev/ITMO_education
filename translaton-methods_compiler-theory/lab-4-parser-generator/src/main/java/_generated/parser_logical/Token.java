package _generated.parser_logical;

public record Token (TypeToken typeToken, String text) {
    @Override
    public String toString() {
        return typeToken.name();
    }
}