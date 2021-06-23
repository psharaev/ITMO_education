import java.io.*;
import java.util.function.Function;

public class FastScanner implements AutoCloseable {

    private final BufferedReader reader;
    private int countLines = 0;

    public FastScanner(BufferedReader reader) {
        this.reader = reader;
    }

    public FastScanner(String s) {
        this(new BufferedReader(new StringReader(s)));
    }

    public FastScanner(InputStream inputStream) {
        this(new BufferedReader(new InputStreamReader(inputStream)));
    }

    public FastScanner(FileReader fileReader) {
        this(new BufferedReader(fileReader));
    }

    public int getLineNumber() {
        return countLines;
    }

    public String next(Function<Integer, Boolean> mask) throws IOException {
        int ch = -1;

        while ((ch = reader.read()) != -1 && !mask.apply(ch)) {
            if (ch == '\n') {
                countLines++;
            }
        }

        if (ch == -1) {
            return null;
        }

        StringBuilder res = new StringBuilder();
        res.appendCodePoint(ch);
        while ((ch = reader.read()) != -1 && mask.apply(ch)) {
            if (ch == '\n') {
                countLines++;
            }

            res.appendCodePoint(ch);
        }

        return res.toString();
    }

    public String next() throws IOException {
        return next(c -> !Character.isWhitespace(c));
    }

    public int nextInt() throws IOException {
        return Integer.parseInt(next(c -> (Character.isDigit(c) || c == '-')));
    }

    public String nextLine() throws IOException {
        countLines++;
        return reader.readLine();
    }

    public void close() throws IOException {
        reader.close();
    }
}