import java.util.Scanner;

public class Reverse {

    private static final int MAX_ARRAY_CAPACITY = 1_000_000;

    public static void main(String[] args) {

        Scanner inputScanner = new Scanner(System.in);
        int[] numbers = new int[MAX_ARRAY_CAPACITY];
        int[] countNumbersInRow = new int[MAX_ARRAY_CAPACITY];

        int numbersSize = 0;
        int countNumbersInRowSize = 0;

        while (inputScanner.hasNextLine()) {

            Scanner lineScanner = new Scanner(inputScanner.nextLine());

            int countNumbers = 0;
            while (lineScanner.hasNext()) {
                numbers[numbersSize++] = Integer.parseInt(lineScanner.next());
                countNumbers++;
            }

            countNumbersInRow[countNumbersInRowSize++] = countNumbers;
        }

        int numberId = numbersSize - 1;
        for (int i = countNumbersInRowSize - 1; i >= 0; i--) {
            for (int j = countNumbersInRow[i] - 1; j >= 0; j--) {
                System.out.print(numbers[numberId--] + " ");
            }
            System.out.println();
        }
    }
}