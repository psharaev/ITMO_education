#include <iostream>

using namespace std;

int min(int a, int b) {
    return a < b ? a : b;
}

int max(int a, int b) {
    return a > b ? a : b;
}

int main() {
    int n, x, y;
    cin >> n >> x >> y;
    n--;

    int l = 0, r = n * max(x, y);
    while (r - l > 1) {
        int m = (l + r) / 2;
        if (m / x + m / y < n)
            l = m;
        else
            r = m;
    }
    cout << r + min(x, y);
    return 0;
}