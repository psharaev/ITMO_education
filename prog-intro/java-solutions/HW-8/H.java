import java.util.Scanner;
import java.util.Map;
import java.util.HashMap;

public class H {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        int n = sc.nextInt();
        int maxArrItem = Integer.MIN_VALUE;

        int[] a = new int[n];
        int[] p = new int[n];

        for (int i = 0; i < n; i++) {
            a[i] = sc.nextInt();
            maxArrItem = Math.max(a[i], maxArrItem);

            p[i] = a[i] + (i == 0 ? 0 : p[i - 1]);
        }

        int[] f = new int[p[n - 1]];
        int r = 0;
        int sum = 0;
        for (int i = 0; i < n; i++) {
            sum += a[i];
            while (r < sum) {
                f[r] = i;
                r++;
            }
        }

        Map<Integer, Integer> m = new HashMap<>();
        int q = sc.nextInt();
        
        for (int i = 0; i < q; i++) {
            int t = sc.nextInt();

            if (t < maxArrItem) {
                System.out.println("Impossible");
                continue;
            }

            if (m.containsKey(t)) {
                System.out.println(m.get(t));
            } else {
                int b = -1;
                int count = 1;
                while (b + t + 1 <= p[n - 1] - 1) {
                    int w = f[b + t + 1] - 1;
                    b = p[w] - 1;
                    count++;
                }
                m.put(t, count);
                System.out.println(count);
            }

        }

    }
}
