package markup;

import java.util.List;

public abstract class NestedMarkup implements Convertable {
    private final String excretionMarkdown;
    private final String excretionBBCode;

    private final List<Convertable> items;

    protected NestedMarkup(String excretionMarkdown, String excretionBBCode, List<Convertable> items) {
        this.excretionMarkdown = excretionMarkdown;
        this.excretionBBCode = excretionBBCode;

        this.items = items;
    }

    @Override
    public void toMarkdown(StringBuilder sb) {
        sb.append(excretionMarkdown);
        for (Convertable item : items) {
            item.toMarkdown(sb);
        }
        sb.append(excretionMarkdown);
    }

    @Override
    public void toBBCode(StringBuilder sb) {
        sb.append("[").append(excretionBBCode).append("]");
        for (Convertable item : items) {
            item.toBBCode(sb);
        }
        sb.append("[/").append(excretionBBCode).append("]");
    }
}