package markup;

public interface Convertable {

    void toMarkdown(StringBuilder sb);

    void toBBCode(StringBuilder sb);
}
