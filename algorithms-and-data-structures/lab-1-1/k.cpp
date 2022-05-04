#include <iostream>
#include <vector>

using namespace std;

bool check(const long long &n, const long long &k, const vector<long long> &a, const long long &m) {
    long long count = 0;
    long long sum = 0;
    for (long long i = 0; i < n; ++i) {
        if (a[i] > m)
            return false;

        sum += a[i];
        if (sum > m) {
            sum = a[i];
            count++;
        }
    }

    return count < k;
}

int main() {
    long long n, k;
    cin >> n >> k;

    long long l = 1, r = 0;

    vector<long long> a(n);
    for (int i = 0; i < n; ++i) {
        cin >> a[i];
        r += a[i];
    }

    long long res = 0;
    while (l <= r) {
        long long m = l + (r - l) / 2;
        if (check(n, k, a, m)) {
            res = m;
            r = m - 1;
        } else {
            l = m + 1;
        }
    }

    cout << res;
}