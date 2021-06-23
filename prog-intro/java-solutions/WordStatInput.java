import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class WordStatInput {

    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("wrong count arguments");
            return;
        }

        Map<String, Integer> words = new LinkedHashMap<>();
        try (BufferedReader inFile = new BufferedReader(new FileReader(args[0], StandardCharsets.UTF_8))) {

            String line;
            while ((line = inFile.readLine()) != null) {
                int startIndex = -1;

                for (int i = 0; i < line.length(); i++) {
                    if (!isInteresting(line.charAt(i))) {
                        if (startIndex != -1) {
                            words.merge(line.substring(startIndex, i).toLowerCase(), 1, Integer::sum);
                            startIndex = -1;
                        }
                    } else if (startIndex == -1) {
                        startIndex = i;
                    }

                }

                if (startIndex != -1) {
                    words.merge(line.substring(startIndex).toLowerCase(), 1, Integer::sum);
                }
            }
        } catch (FileNotFoundException e) {
            System.err.println("Error not found input file: " + args[0]);
        } catch (IOException e) {
            System.err.println("Error read input file: " + args[0]);
        }

        try (BufferedWriter out = new BufferedWriter(new FileWriter(args[1], StandardCharsets.UTF_8))) {
            for (Map.Entry<String, Integer> entry : words.entrySet()) {
                out.write(entry.getKey() + " " + entry.getValue() + "\n");
            }
        } catch (IOException e) {
            System.err.println("Error write output file: " + args[1]);
        }
    }

    private static boolean isInteresting(final char c) {
        return Character.isLetter(c) || Character.getType(c) == Character.DASH_PUNCTUATION || c == '\'';
    }
}
