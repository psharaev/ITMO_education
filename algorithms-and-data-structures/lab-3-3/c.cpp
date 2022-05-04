#include <iostream>
#include <vector>

using namespace std;

typedef unsigned long long ull;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    string s;
    cin >> s;

    ull n = (ull) s.length();

    vector<ull> z(n);
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

    for (size_t i = 1; i < z.size(); ++i) {
        cout << z[i] << ' ';
    }

    return 0;
}