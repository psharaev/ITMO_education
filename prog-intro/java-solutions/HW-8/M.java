import java.util.Scanner;
import java.util.Map;
import java.util.HashMap;

public class M {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        int t = sc.nextInt();

        for (int caseNumber = 0; caseNumber < t; caseNumber++) {
            int n = sc.nextInt();
            int[] a = new int[n];

            for (int j = 0; j < n; j++) {
                a[j] = sc.nextInt();
            }

            int count = 0;
            for (int i = 0; i < n - 1; i++) {
                Map<Integer, Integer> m = new HashMap<>();
                
                for (int j = n - 1; j > i; j--) {
                    int key = 2 * a[j] - a[i];

                    if (m.containsKey(key)) {
                        count += m.get(key);
                    }

                    m.put(a[j], m.containsKey(a[j]) ? m.get(a[j]) + 1 : 1);
                }
            }

            System.out.println(count);
        }
    }
}
