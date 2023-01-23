import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */

public class Tree {
    public final String text;
    public final List<Tree> children;

    public Tree(final String text, final Tree... children) {
        this.text = text;
        this.children = Arrays.asList(children);
    }

    public Tree(final String text) {
        this.text = text;
        children = Collections.emptyList();
    }

    @Override
    public String toString() {

        final String collect;
        if (children == null) {
            collect = "null";
        } else {
            collect = children.stream().map(Objects::toString).collect(Collectors.joining(", ", "[", "]"));
        }
        return text + ": " + collect;
    }
}
