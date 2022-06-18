#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

typedef long long ll;

typedef long long ll;
typedef vector<ll> vi;

#define MOD 1000000007

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    size_t n, m;
    cin >> n >> m;

    vi c(n);
    for (size_t i = 0; i < n; i++) {
        cin >> c[i];
    }
    sort(c.begin(), c.end());

    vi res(m + 1, 0), pref(m + 1, 0);
    res[0] = 1ll;
    for (size_t i = 0; i < n; i++) {
        res[c[i]] = 1ll;
    }

    pref[0] = 1;
    for (size_t i = 1; i <= m; i++) {
        for (size_t j = 0; j < n; j++) {
            if (i <= c[j]) {
                break;
            }
            res[i] = (res[i] + pref[i - c[j]]) % MOD;
        }
        for (size_t j = 0; j <= i; j++) {
            pref[i] = (pref[i] + res[j] * res[i - j]) % MOD;
        }
    }

    for (size_t i = 1; i <= m; i++) {
        cout << res[i] << ' ';
    }

    return 0;
}