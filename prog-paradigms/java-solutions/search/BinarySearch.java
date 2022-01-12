package search;

public class BinarySearch {

    public static void main(String[] args) {
        int x = Integer.parseInt(args[0]);
        int arrSize = args.length - 1;

        int[] arr = new int[arrSize];
        for (int i = 0; i < arrSize; i++) {
            arr[i] = Integer.parseInt(args[i + 1]);
        }

        int recursive = recursiveBinarySearch(x, arr, -1, arr.length);
        int iterative = iterativeBinarySearch(x, arr);

        if (recursive == iterative) {
            System.out.println(recursive);
        } else {
            System.err.format("discrepancy results: recursive: %d, iterative: %d\n", recursive, iterative);
        }
    }

    // INV: (∀i,j i>j: arr[i] >= arr[j]) && arr[-1] == +INF && arr[arr.length] == -INF

    // Pred: INV && l >= -1 && r <= arr.length && l <= r
    // Post: a[result] <= x && result -- minimum && 0 <= result <= arr.length
    public static int recursiveBinarySearch(final int x, final int[] arr, final int l, final int r) {
        if (l < r - 1) {
            // l < r - 1
            final int m = (l + r) / 2;
            // (l < r - 1 && m = (l + r) / 2) -> l < m < r
            if (arr[m] > x) {
                // a[m] > x -> result > m
                return recursiveBinarySearch(x, arr, m, r);
                // result == recursiveBinarySearch(x, arr, m, r)
            } else {
                // a[m] <= x -> result <= m
                return recursiveBinarySearch(x, arr, l, m);
                // result == recursiveBinarySearch(x, arr, l, m)
            }
        }
        // (-1 < l < result <= r <= a.length) && (r - l == 1) -> (r == result)

        return r;
    }

    // Pred: INV
    // Post: a[result] <= x && result -- minimum && 0 <= result <= arr.length
    public static int iterativeBinarySearch(final int x, final int[] arr) {
        int l = -1;
        int r = arr.length;
        // l = -1 && r = a.length && l <= r - 1

        // Inv: -1 <= l < result <= r <= a.length && r - l > 0
        while (l < r - 1) {
            // l < r - 1
            final int m = (l + r) / 2;
            // (l < r - 1 && m = (l + r) / 2) -> l < m < r
            if (arr[m] > x) {
                // a[m] > x -> result > m
                l = m;
                // l' = (l + r) / 2 && result > l'
            } else {
                // a[m] <= x -> result <= m
                r = m;
                // r' = (l + r) / 2 && result <= r'
            }
            // (l' > l || r' < r) && (l < m < r) -> (0 < r' - l' < r - l)
        }
        // (-1 < l < result <= r <= a.length) && (r - l == 1) -> (r == result)

        return r;
    }
}