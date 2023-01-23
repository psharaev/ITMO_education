import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * @author Pavel Sharaev (mail@pechhenka.ru)
 */
public class Main {
    public static void main(final String[] args) {
        final Scanner scanner = new Scanner(System.in);
        System.out.println("Enter expression or 0 to quit");
        while (true) {
            final String s = scanner.nextLine();
            if ("0".equals(s)) {
                break;
            }
            try {
                dump(new Parser().parse(s), new ArrayList<>(),  0);
            } catch (final ParseException e) {
                System.err.println("Exception: " + e.getMessage());
            }
        }
    }

    private static void dump(final Tree root, final ArrayList<Integer> needLine, final int depth) {
        printPrefix(needLine);
        if (depth > 0) {
            final int lastC = needLine.get(depth - 1);
            if (lastC > 0) {
                needLine.set(depth - 1, lastC - 1);
            }
        }
        System.out.println(root.text);

        if (root.children.size() > 0) {
            needLine.add(root.children.size());
            final List<Tree> childs = root.children;
            for (final Tree child : childs) {
                printSpace(needLine);
                dump(child, needLine, depth + 1);
            }
            needLine.remove(needLine.size() - 1);
        }
    }

    private static void printPrefix(final ArrayList<Integer> needLine) {
        for (int i = 0; i < needLine.size(); i++) {
            final int c = needLine.get(i);
            if (c == 0) {
                System.out.print("   ");
            } else if (i + 1 == needLine.size()) {
                System.out.print("|--");
            } else {
                System.out.print("|  ");
            }
        }
    }

    private static void printSpace(final ArrayList<Integer> needLine) {
        for (final int c : needLine) {
            if (c > 0) {
                System.out.print("|  ");
            } else {
                System.out.print("   ");
            }
        }
        System.out.println();
    }
}