package md2html;

import java.util.*;

public class MdParagraphParser {

    private final static TreeMap<String, String> md2htmlTags = new TreeMap<>(Map.of(
            "*", "em", "_", "em", // выделение
            "**", "strong", "__", "strong", // сильное выделение
            "--", "s", // зачёркивание
            "`", "code", // код
            "~", "mark")); // выделение цвтом

    private final static Map<Character, String> htmlSymbols = Map.of(
            '<', "&lt;",
            '>', "&gt;",
            '&', "&amp;");

    private final static Set<Character> shieldedMdTags = Set.of('*', '-', '_', '`', '~');

    private final List<String> mdTags;

    private String line;
    private int size;
    private int pos;

    public MdParagraphParser() {
        mdTags = List.copyOf(md2htmlTags.descendingKeySet());
    }

    public String parse(final String line) {
        this.line = line;
        this.size = line.length();
        this.pos = 0;
        return parse();
    }

    private String parse() {
        StringBuilder res = new StringBuilder();
        final int headerLevel = parseHeader();
        if (headerLevel > 0) {
            res.append("<h").append(headerLevel).append(">");
        } else {
            res.append("<p>");
        }

        res.append(parseMdTag(""));

        if (headerLevel > 0) {
            res.append("</h").append(headerLevel).append(">");
        } else {
            res.append("</p>");
        }
        return res.toString();
    }

    private StringBuilder parseMdTag(final String lastMdTag) {
        StringBuilder sub = new StringBuilder();

        while (pos < size) {
            char ch = line.charAt(pos);
            boolean flag = true;

            if (ch == '\\') {
                pos++;
                char nextChar = pos < size ? line.charAt(pos) : '\\';
                sub.append(nextChar);
                if (shieldedMdTags.contains(nextChar)) {
                    pos++;
                }
                flag = false;
            } else {
                for (final String tag : mdTags) {
                    if (checkTag(tag)) {
                        pos += tag.length();
                        if (lastMdTag.equals(tag)) {
                            wrap(sub, tag);
                            return sub;
                        }
                        sub.append(parseMdTag(tag));
                        flag = false;
                    }
                }
            }

            if (flag) {
                String res = htmlSymbols.get(ch);
                if (res != null) {
                    sub.append(res);
                } else {
                    sub.append(ch);
                }
                pos++;
            }
        }

        sub.insert(0, lastMdTag);
        return sub;
    }

    private boolean checkTag(final String mdTag) {
        if (pos + mdTag.length() > size) {
            return false;
        }

        for (int i = 0; i < mdTag.length(); i++) {
            if (line.charAt(pos + i) != mdTag.charAt(i)) {
                return false;
            }
        }
        return true;
    }


    private int parseHeader() {
        int res = 0;
        while (res < size && line.charAt(res) == '#') {
            res++;
        }

        if (res > 0 && res < size && line.charAt(res) == ' ') {
            pos = res + 1;
            return res;
        }

        return 0;
    }

    private void wrap(final StringBuilder sb, final String mdTag) {
        sb.insert(0, "<" + md2htmlTags.get(mdTag) + ">"); // start html tag
        sb.append("</").append(md2htmlTags.get(mdTag)).append(">"); // close html tag
    }
}