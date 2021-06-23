import java.util.Scanner;

public class I {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        long n = sc.nextInt();

        long xl = Long.MAX_VALUE, xr = Long.MIN_VALUE;
        long yl = Long.MAX_VALUE, yr = Long.MIN_VALUE;

        for (int i = 0; i < n; i++) {
            long x = sc.nextInt();
            long y = sc.nextInt();
            long h = sc.nextInt();

            xl = Math.min(xl, x - h);
            xr = Math.max(xr, x + h);
            yl = Math.min(yl, y - h);
            yr = Math.max(yr, y + h);
        }

        System.out.printf("%d %d %d", (xl + xr) / 2,
                (yl + yr) / 2,
                div_up(Math.max(xr - xl, yr - yl), 2));
    }

    private static long div_up(long a, long b) {
        return (a - 1) / b + 1;
    }
}