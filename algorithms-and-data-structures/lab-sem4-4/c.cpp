#include <iostream>
#include <cmath>

using namespace std;

typedef long long ll;

ll _gcd(const ll &a, const ll &b, ll &x, ll &y) {
    if (a == 0) {
        x = 0;
        y = 1;
        return b;
    }
    ll x1, y1;
    ll g = _gcd(b % a, a, x1, y1);
    x = y1 - (b / a) * x1;
    y = x1;
    return g;
}

void calc(const ll &a, const ll &b, ll c, ll &x0, ll &y0) {
    ll g = _gcd(abs(a), abs(b), x0, y0);
    x0 *= c / g;
    y0 *= c / g;
    if (a < 0) {
        x0 *= -1;
    }
    if (b < 0) {
        y0 *= -1;
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    ll a, b, n, m;
    cin >> a >> b >> n >> m;

    ll x, y;

    calc(n, -m, b - a, x, y);

    ll res = (a + x * n);

    ll mn = m * n;
    while (res < 0) {
        res += mn;
    }
    while (res > mn) {
        res -= mn;
    }

    cout << res << '\n';

    return 0;
}
