package markup;

import java.util.List;

public class Strikeout extends NestedMarkup {
    public Strikeout(List<Convertable> items) {
        super("~", "s", items);
    }
}
