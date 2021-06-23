import java.util.Scanner;

public class A {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        long a = sc.nextLong();
        long b = sc.nextLong();
        long n = sc.nextLong();

        System.out.println(2 * div_up(n-b,b-a) + 1);
    }

    private static long div_up(long a, long b) {
        return (a - 1) / b + 1;
    }
}