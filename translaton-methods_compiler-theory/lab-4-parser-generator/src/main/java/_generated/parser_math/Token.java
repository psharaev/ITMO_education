package _generated.parser_math;

public record Token (TypeToken typeToken, String text) {
    @Override
    public String toString() {
        return typeToken.name();
    }
}