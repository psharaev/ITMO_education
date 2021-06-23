package markup;

import java.util.List;

public class Emphasis extends NestedMarkup {
    public Emphasis(List<Convertable> items) {
        super("*", "i", items);
    }
}
