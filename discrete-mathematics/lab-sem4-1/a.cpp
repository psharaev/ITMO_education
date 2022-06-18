#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

typedef long long ll;
typedef vector<ll> vi;

#define MOD 998244353

struct polynomial {
    vector<ll> a;

    explicit polynomial(const vector<ll> &a) : a(a) {}

    explicit polynomial(vector<ll> &&a): a(a) {}

    explicit polynomial(const size_t &degree) {
        a = vector<ll>(degree + 1);
        for (size_t i = 0; i <= degree; ++i) {
            cin >> a[i];
        }
    }

    ll operator[](const size_t &index) const {
        if (index < a.size())
            return a[index];
        return 0;
    }

    [[nodiscard]] size_t size() const {
        return a.size();
    }

    [[nodiscard]] size_t degree() const {
        return a.size() - 1;
    }

    void dumpWithDegree() const {
        cout << degree() << '\n';
        for (const ll &item: a) {
            cout << item << ' ';
        }
        cout << '\n';
    }

    void dumpWithoutDegree() const {
        for (const ll &item: a) {
            cout << item << ' ';
        }
        cout << '\n';
    }
};

polynomial operator+(const polynomial &p, const polynomial &q) {
    size_t n = p.size(), m = q.size();
    vi res;

    if (n < m) {
        res = q.a;
    } else {
        res = p.a;
    }

    size_t min_size = min(n, m);

    for (size_t i = 0; i < min_size; ++i) {
        res[i] = (p[i] + q[i]) % MOD;
    }

    return polynomial(move(res));
}

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

polynomial operator/(const polynomial &p, const polynomial &q) {
    vi res(1000, 0);
    res[0] = p[0] / q[0];
    for (int i = 1; i < 1000; ++i) {
        ll sum = 0;
        for (int j = 0; j < i; ++j) {
            sum = (sum + res[j] * q[i - j]) % MOD;
        }
        res[i] = ((MOD + p[i] - sum) % MOD) / q[0];
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
    polynomial q(m);

    (p + q).dumpWithDegree();
    (p * q).dumpWithDegree();
    (p / q).dumpWithoutDegree();

    return 0;
}

/*
3 2
0 1 2 3
1 2 3

1 3
1 2
1 4 5 2
 */
