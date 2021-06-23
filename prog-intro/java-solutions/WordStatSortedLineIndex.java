import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Map;
import java.util.TreeMap;

public class WordStatSortedLineIndex {

    private static class StringOccurrence {
        private final int lineNumber;
        private final int entryNumber;

        public StringOccurrence(int lineNumber, int entryNumber) {
            this.lineNumber = lineNumber;
            this.entryNumber = entryNumber;
        }

        @Override
        public String toString() {
            return lineNumber + ":" + entryNumber;
        }
    }

    private static class WordStatistics {

        private final ArrayList<StringOccurrence> occurrences;

        public WordStatistics() {
            this.occurrences = new ArrayList<>();
        }

        public void add(final StringOccurrence occurrence) {
            this.occurrences.add(occurrence);
        }
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("wrong count arguments");
            return;
        }

        Map<String, WordStatistics> words = new TreeMap<>();
        try (FastScanner input = new FastScanner(new FileReader(args[0], StandardCharsets.UTF_8))) {
            int quantity = 0, lastLine = input.getLineNumber();
            String word;
            while ((word = input.next(c -> (Character.isLetter(c) ||
                    Character.getType(c) == Character.DASH_PUNCTUATION ||
                    c == '\''))) != null) {
                final int currentLine = input.getLineNumber();
                if (lastLine != currentLine) {
                    quantity = 0;
                }

                word = word.toLowerCase();
                quantity++;

                WordStatistics s = words.get(word);
                if (s == null) {
                    s = new WordStatistics();
                    words.put(word, s);
                }

                s.add(new StringOccurrence(currentLine + 1, quantity));
                lastLine = currentLine;
            }
        } catch (FileNotFoundException e) {
            System.err.println("Error not found input file " + args[0]);
        } catch (IOException e) {
            System.err.println("Error read input file: " + args[0]);
        }

        try (BufferedWriter out = new BufferedWriter(new FileWriter(args[1], StandardCharsets.UTF_8))) {
            for (Map.Entry<String, WordStatistics> word : words.entrySet()) {
                WordStatistics wordStatistics = word.getValue();
                out.write(word.getKey() + " " + wordStatistics.occurrences.size());

                for (StringOccurrence item : wordStatistics.occurrences) {
                    out.write(' ');
                    out.write(item.toString());
                }

                out.write('\n');
            }
        } catch (IOException e) {
            System.err.println("Error write output file: " + args[1]);
        }
    }
}