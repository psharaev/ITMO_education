package markup;

import java.util.List;

public class Strong extends NestedMarkup {
    public Strong(List<Convertable> items) {
        super("__", "b", items);
    }
}
