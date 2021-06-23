package markup;

import java.util.List;

public class Paragraph implements Convertable {

    private final List<Convertable> items;

    public Paragraph(List<Convertable> items) {
        this.items = items;
    }

    @Override
    public void toMarkdown(StringBuilder sb) {
        for (Convertable item : items) {
            item.toMarkdown(sb);
        }
    }

    @Override
    public void toBBCode(StringBuilder sb) {
        for (Convertable item : items) {
            item.toBBCode(sb);
        }
    }
}
