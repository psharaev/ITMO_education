package search;

public class BinarySearchMin {

    public static void main(String[] args) {
        int[] arr = new int[args.length];
        for (int i = 0; i < args.length; i++) {
            arr[i] = Integer.parseInt(args[i]);
        }

        int recursive = recursiveBinarySearchMin(arr, -1, arr.length);
        int iterative = iterativeBinarySearchMin(arr);

        if (recursive == iterative) {
            System.out.println(recursive);
        } else {
            System.err.format("discrepancy results: recursive: %d, iterative: %d\n", recursive, iterative);
        }
    }

    // ARR: ∃!k: ∀i [0..k-1] arr[i] < arr[i+1] ∀j [k..n-2] arr[j] < arr[j+1] && arr[k] > arr[k+1] && arr[n-1] < arr[0]

    // Pred: ARR && l >= -1 && r <= arr.length && l + 1 <= r && l < k <= r
    // Post: R == arr[k]
    public static int recursiveBinarySearchMin(final int[] arr, int l, int r) {
        // l = -1 && r = a.length && l <= r - 1
        if (l < r - 1) {
            // l < r - 1
            final int m = (l + r) / 2;
            // (l < r - 1 && m = (l + r) / 2) -> l < m < r
            if (arr[m] > arr[arr.length - 1]) {
                // a[m] > arr[n-1] -> k > m
                return recursiveBinarySearchMin(arr, m, r);
                // R == recursiveBinarySearchMin(arr, m, r)
            } else {
                // a[m] <= arr[n-1] -> k <= m
                return recursiveBinarySearchMin(arr, l, m);
                // R == recursiveBinarySearchMin(arr, l, m)
            }
        }
        // (-1 < l < k <= r <= a.length) && (r - l == 1) -> (r == k)

        return arr[r];
    }

    // Pred: ARR
    // Post: R == arr[k]
    public static int iterativeBinarySearchMin(final int[] arr) {
        final int n = arr.length;

        int l = -1;
        int r = n;

        // l = -1 && r = a.length && l <= r - 1

        // INV: -1 <= l < k <= r <= a.length && r - l > 0
        while (l < r - 1) {
            // l < r - 1
            int m = (l + r) / 2;
            // (l < r - 1 && m = (l + r) / 2) -> l < m < r
            if (arr[m] > arr[n - 1]) {
                // a[m] > arr[n-1] -> k > m
                l = m;
                // l' = (l + r) / 2 && k > l'
            } else {
                // a[m] <= arr[n-1] -> k <= m
                r = m;
                // r' = (l + r) / 2 && k <= r'
            }
            // (l' > l || r' < r) && (l < m < r) -> (0 < r' - l' < r - l)
        }
        // (-1 < l < k <= r <= a.length) && (r - l == 1) -> (r == k)

        return arr[r];
    }
}