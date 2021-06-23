package md2html;

import java.io.*;
import java.nio.charset.StandardCharsets;

public class Md2Html {

    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("Expected 2 arguments");
            return;
        }

        MdParagraphParser paragraphParser = new MdParagraphParser();
        StringBuilder paragraph = new StringBuilder();

        try (BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(new FileInputStream(args[0]), StandardCharsets.UTF_8));
             BufferedWriter bufferedWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1]), StandardCharsets.UTF_8))) {
            while (true) {
                String line = bufferedReader.readLine();
                if (line == null || line.isEmpty()) {
                    if (paragraph.length() > 0) {
                        bufferedWriter.write(paragraphParser.parse(paragraph.toString()));
                        bufferedWriter.write(System.lineSeparator());
                        paragraph.setLength(0);
                    }
                    if (line == null) {
                        break;
                    }
                } else {
                    if (paragraph.length() > 0) {
                        paragraph.append('\n');
                    }
                    paragraph.append(line);
                }
            }
        } catch (FileNotFoundException e) {
            System.out.println("No such file " + e.getMessage());
        } catch (
                IOException e) {
            System.out.println("Error reading input file: " + e.getMessage());
        }
    }
}
