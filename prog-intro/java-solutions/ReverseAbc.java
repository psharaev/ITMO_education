import java.io.IOException;

public class ReverseAbc {

    private static final int MAX_ARRAY_CAPACITY = 10_000_000;
    private static final char ABC_TO_INT = (char) ('a' - '0');

    public static void main(String[] args) {

        int[] numbers = new int[MAX_ARRAY_CAPACITY];
        int[] countNumbersInRow = new int[MAX_ARRAY_CAPACITY];

        int numbersSize = 0;
        int countNumbersInRowSize = 0;

        try (FastScanner inputScanner = new FastScanner(System.in)) {

            String num;
            int countNumbers = 0, lastLine = inputScanner.getLineNumber();
            while ((num = inputScanner.next()) != null) {
                int currentLine = inputScanner.getLineNumber();
                if (lastLine != currentLine) {
                    countNumbersInRow[lastLine] = countNumbers;
                    countNumbers = 0;
                }

                numbers[numbersSize++] = abcToInt(num);
                countNumbers++;

                lastLine = currentLine;
            }

            countNumbersInRowSize = inputScanner.getLineNumber();
            countNumbersInRow[lastLine] = countNumbers;
        } catch (IOException e) {
            e.printStackTrace();
        }
        int numberId = numbersSize - 1;
        for (int i = countNumbersInRowSize - 1; i >= 0; i--) {
            for (int j = countNumbersInRow[i] - 1; j >= 0; j--) {
                System.out.print(intToAbc(numbers[numberId--]) + " ");
            }
            System.out.println();
        }
    }

    private static int abcToInt(String str) {
        boolean negative = false;
        int i = 0;
        if (str.charAt(0) == '-') {
            negative = true;
            i = 1;
        }
        int res = 0;
        for (int end = str.length(); i < end; i++) {
            res = (res * 10) + (str.charAt(i) - 'a');
        }
        return negative ? -res : res;
    }

    private static String intToAbc(int t) {
        String in = Integer.toString(t);
        StringBuilder res = new StringBuilder();
        for (int i = 0, end = in.length(); i < end; i++) {
            if (in.charAt(i) != '-') {
                res.append((char) (in.charAt(i) + ABC_TO_INT));
            } else {
                res.append('-');
            }
        }
        return res.toString();
    }
}