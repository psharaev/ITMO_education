package _generated.parser_logical;

import java.util.ArrayList;
import java.util.List;

public class Tree {
    private final String node;
    private final List<Tree> children;

    public Tree(String node) {
        this.node = node;
        this.children = new ArrayList<>();
    }

    public void addChild(Tree node) {
        children.add(node);
    }

    public void addChild(String node) {
        children.add(new Tree(node));
    }

    public String getNode() {
        return node;
    }

    public List<Tree> getChildren() {
        return children;
    }
}
