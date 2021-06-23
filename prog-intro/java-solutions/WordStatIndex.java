import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;

public class WordStatIndex {

    private static class WordStatistics {
        private final ArrayList<Integer> occurrences;

        public WordStatistics() {
            this.occurrences = new ArrayList<>();
        }

        public void add(final int occurrence) {
            this.occurrences.add(occurrence);
        }
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("wrong count arguments");
            return;
        }

        Map<String, WordStatistics> words = new LinkedHashMap<>();
        try (FastScanner input = new FastScanner(new FileReader(args[0], StandardCharsets.UTF_8))) {
            int occurrenceNumber = 0;
            String word;
            while ((word = input.next(c -> (Character.isLetter(c) ||
                    Character.getType(c) == Character.DASH_PUNCTUATION ||
                    c == '\''))) != null) {
                word = word.toLowerCase();
                occurrenceNumber++;

                WordStatistics s = words.get(word);
                if (s == null) {
                    s = new WordStatistics();
                    words.put(word, s);
                }

                s.add(occurrenceNumber);
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

                for (Integer occurrence : wordStatistics.occurrences) {
                    out.write(' ');
                    out.write(occurrence.toString());
                }

                out.write('\n');
            }
        } catch (IOException e) {
            System.err.println("Error write output file: " + args[1]);
        }
    }
}