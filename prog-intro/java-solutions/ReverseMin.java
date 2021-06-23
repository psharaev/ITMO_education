import java.util.Arrays;
import java.util.Scanner;

public class ReverseMin {

    private static final int MAX_ARRAY_CAPACITY = 1_000_000;

    public static void main(String[] args) {

        Scanner inputScanner = new Scanner(System.in);
        int[] numbers = new int[MAX_ARRAY_CAPACITY];
        int[] countNumbersInRow = new int[MAX_ARRAY_CAPACITY];
        int[] minInRow = new int[MAX_ARRAY_CAPACITY];
        int[] minInColumn = new int[MAX_ARRAY_CAPACITY];

        int numbersSize = 0;
        int countNumbersInRowSize = 0;
        int minInRowSize = 0;
        int minInColumnSize = 0;

        while (inputScanner.hasNextLine()) {

            Scanner lineScanner = new Scanner(inputScanner.nextLine());

            minInRow[minInRowSize] = Integer.MAX_VALUE;

            int countNumbers = 0;
            while (lineScanner.hasNext()) {
                int t = Integer.parseInt(lineScanner.next());
                if (minInRow[minInRowSize] > t) {
                    minInRow[minInRowSize] = t;
                }
                numbers[numbersSize++] = t;
                countNumbers++;
            }

            if (minInColumnSize < countNumbers) {
                minInColumnSize = countNumbers;
            }

            countNumbersInRow[countNumbersInRowSize++] = countNumbers;
            minInRowSize++;
        }

        Arrays.fill(minInColumn, 0, minInColumnSize, Integer.MAX_VALUE);

        int numberId = 0;
        for (int i = 0; i < countNumbersInRowSize; i++) {
            for (int j = 0; j < countNumbersInRow[i]; j++) {
                if (minInColumn[j] > numbers[numberId]) {
                    minInColumn[j] = numbers[numberId];
                }
                numberId++;
            }
        }

        for (int i = 0; i < countNumbersInRowSize; i++) {
            for (int j = 0; j < countNumbersInRow[i]; j++) {
                int m = Math.min(minInRow[i], minInColumn[j]);
                System.out.print(m + " ");
            }
            System.out.println();
        }
    }
}