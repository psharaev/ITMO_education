#include <iostream>


using namespace std;

typedef __int128 ll;

ll binPow(ll a, ll n, const ll &mod) {
    ll res = 1;
    while (n > 0) {
        if (n & 1) {
            res *= a;
            res %= mod;
            --n;
        } else {
            a *= a;
            a %= mod;
            n /= 2;
        }
    }
    return res;
}

void _gcd(const ll &a, const ll &b, ll &d, ll &x, ll &y) {
    if (b == 0) {
        d = a;
        x = 1;
        y = 0;
        return;
    }
    ll d1, x1, y1;
    _gcd(b, a % b, d1, x1, y1);
    d = d1;
    x = y1;
    y = x1 - y1 * (a / b);
}

ll invert(const ll &a, const ll &mod) {
    ll g, x, y;
    _gcd(a, mod, g, x, y);
    x /= g;
    if (x < 0) {
        x += mod;
    }
    return x;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    long long n, e, c;
    cin >> n >> e >> c;
    ll p = 1, q = 1;
    for (ll i = 2; i < n; i++) {
        if (n % i == 0) {
            p = i;
            q = n / i;
            break;
        }
    }

    cout << (long long) binPow(c, invert(e, (p - 1) * (q - 1)), n) << '\n';

    return 0;
}
