#include <vector>
#include <iostream>
#include <algorithm>

using namespace std;

typedef long long ll;
typedef vector<ll> vi;

#define MOD 1000000000000LL

struct rational_polynomial {
    vector<ll> p;
    vector<ll> q;
    size_t degreeQ;
    size_t degreeP;

    explicit rational_polynomial(const size_t &k, const vi &a, const vi &c) {
        p = vi(k + 1, 0);
        q = vi(k + 1, 0);
        p[0] = a[0];

        q[0] = 1;
        for (size_t i = 1; i <= k; i++) { // calc Q
            q[i] = -c[i - 1];
        }

        for (size_t i = 1; i < k; i++) { // calc P
            ll res = 0;
            for (int j = 1; j <= i; j++) {
                res = (res + a[i - j] * q[j]) % MOD;
            }
            p[i] = (a[i] + res) % MOD;
        }

        this->degreeQ = k;
        degreeP = 0;
        for (size_t i = k - 1; i > 0; i--) { // calc degree
            if (p[i] != 0) {
                degreeP = i;
                break;
            }
        }
    }

    void dumpP() const {
        cout << degreeP << '\n';
        for (size_t i = 0; i <= degreeP; i++) {
            cout << p[i] << ' ';
        }
        cout << '\n';
    }

    void dumpQ() const {
        cout << degreeQ << '\n';
        for (int i = 0; i <= degreeQ; i++) {
            cout << q[i] << ' ';
        }
        cout << '\n';
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);
    size_t k;
    cin >> k;

    vector<ll> a(k), c(k);
    for (int i = 0; i < k; i++) {
        cin >> a[i];
    }
    for (int i = 0; i < k; i++) {
        cin >> c[i];
    }

    rational_polynomial rp(k, a, c);

    rp.dumpP();
    rp.dumpQ();

    return 0;
}