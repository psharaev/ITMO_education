import java.math.BigInteger;
import java.util.Scanner;

public class Main {
    public static void main(final String[] args) {
        final Scanner in = new Scanner(System.in);
        final int t = Integer.parseInt(in.nextLine());
        for (int i = 0; i < t; ++i) {
            System.out.println(isPrime(in.nextLine()) ? "YES" : "NO");
        }
    }

    private static boolean isPrime(final String num) {
        final BigInteger b = new BigInteger(num);
        return b.isProbablePrime(10);
    }
}