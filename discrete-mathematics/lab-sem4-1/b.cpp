#include <iostream>
#include <vector>

using namespace std;

typedef long long ll;
typedef vector<ll> vi;

const ll MOD = 998244353LL;

ll _gcd(const ll &a, const ll &b, ll &x, ll &y) {
    if (a == 0) {
        x = 0;
        y = 1;
        return b;
    }
    ll x1, y1;
    ll d = _gcd(b % a, a, x1, y1);
    x = (y1 - ((b / a) * x1));
    y = x1;
    return d;
}

ll getInversed(ll a) {
    ll result;
    ll x, y, d;
    d = _gcd(a, MOD, x, y);
    if (d != 1) {
        result = 1;
    } else {
        result = (x + MOD) % MOD;
    }
    return result;
}

struct polynomial;

polynomial operator*(const polynomial &p, const polynomial &q);

struct polynomial {
    vector<ll> a;

    vector<polynomial> pows = vector<polynomial>();

    explicit polynomial(const vector<ll> &a) : a(a) {}

    explicit polynomial(vector<ll> &&a) : a(a) {}

    explicit polynomial(const size_t &degree) {
        a = vector<ll>(degree + 1);
        for (size_t i = 0; i <= degree; ++i) {
            cin >> a[i];
        }
    }

    static polynomial empty() {
        return polynomial(vi(1, 1));
    }

    ll operator[](const size_t &index) const {
        if (index < a.size())
            return a[index];
        return 0;
    }

    [[nodiscard]] size_t size() const {
        return a.size();
    }

    void dumpWithoutDegree() const {
        for (const ll &item: a) {
            cout << item << ' ';
        }
        cout << '\n';
    }

    polynomial &getPow(const size_t &p) {
        if (pows.empty()) {
            pows.push_back(polynomial::empty());
        }

        if (pows.size() == 1) {
            pows.push_back((*this));
        }

        if (p >= pows.size()) {
            for (size_t i = pows.size(); i <= p; ++i) {
                pows.push_back((*this) * pows[i - 1]);
            }
        }

        return pows[p];
    }

    polynomial exp(const size_t &size) {
        vi c(size + 1);
        c[0] = 1;

        for (int i = 1; i <= size; i++) {
            c[i] = c[i - 1];
            c[i] = (c[i] * getInversed(i)) % MOD;
        }

        vi res(size);
        for (size_t i = 0; i < size; ++i) {
            ll k = 0;
            for (size_t j = 0; j < size; ++j) {
                k = (k + c[j] * getPow(j)[i]) % MOD;
            }
            res[i] = k;
        }

        return polynomial(move(res));
    }

    polynomial log(const size_t &size) {
        vi c(size + 1);
        c[1] = 1;
        for (int i = 2; i <= size; i++) {
            c[i] = (-c[i - 1] + MOD) % MOD;
            c[i] = (c[i] * (i - 1)) % MOD;
            c[i] = (c[i] * getInversed(i)) % MOD;
        }

        vi res(size);
        for (size_t i = 0; i < size; ++i) {
            ll k = 0;
            for (size_t j = 0; j < size; ++j) {
                k = (k + c[j] * getPow(j)[i]) % MOD;
            }
            res[i] = k;
        }

        return polynomial(move(res));
    }

    polynomial sqrt(const size_t &size) {
        vi c(size + 1);
        c[0] = 1;

        for (int i = 1; i <= size; i++) {
            c[i] = (-c[i - 1] + MOD) % MOD;
            c[i] *= ((2 * i - 1) * (2 * i) * (3 - 2 * i) + MOD) % MOD;
            c[i] %= MOD;
            c[i] *= getInversed((i * i * 4 * (1 - 2 * i) + MOD) % MOD);
            c[i] %= MOD;
        }

        vi res(size);
        for (size_t i = 0; i < size; ++i) {
            ll k = 0;
            for (size_t j = 0; j < size; ++j) {
                k = (k + c[j] * getPow(j)[i]) % MOD;
            }
            res[i] = k;
        }

        return polynomial(move(res));
    }
};

polynomial operator*(const polynomial &p, const polynomial &q) {
    size_t n = p.size(), m = q.size();
    vi res(n + m - 1, 0);

    for (size_t i = 0; i < n; ++i) {
        for (size_t j = 0; j < m; ++j) {
            res[i + j] = (res[i + j] + p[i] * q[j]) % MOD;
        }
    }

    return polynomial(move(res));
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    size_t n, m;
    cin >> n >> m;

    polynomial p(n);

    p.sqrt(m).dumpWithoutDegree();
    p.exp(m).dumpWithoutDegree();
    p.log(m).dumpWithoutDegree();

    return 0;
}