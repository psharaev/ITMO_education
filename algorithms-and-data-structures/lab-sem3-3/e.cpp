#include <iostream>
#include <vector>

using namespace std;

typedef unsigned long long ull;

typedef vector<ull> vi;

vi z_func(const string &s) {
    ull n = (ull) s.length();
    vi z(n);
    for (ull i = 1, l = 0, r = 0; i < n; ++i) {
        if (i <= r) {
            z[i] = min(r - i + 1, z[i - l]);
        }
        while (i + z[i] < n && s[z[i]] == s[i + z[i]]) {
            ++z[i];
        }
        if (i + z[i] - 1 > r) {
            l = i, r = i + z[i] - 1;
        }
    }
    return z;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    string s;
    cin >> s;
    vi z = std::move(z_func(s));

    size_t n = s.length();

    for (ull i = 1; i < n; ++i) {
        if (n % i == 0 && z[i] == n - i) {
            cout << i << '\n';
            return 0;
        }
    }

    cout << n << '\n';

    return 0;
}