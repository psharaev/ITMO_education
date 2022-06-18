#include <iostream>
#include <vector>

using namespace std;

typedef long long ll;
typedef vector<ll> vi;

#define MOD 998244353

const int MAXM = 5001;

struct polynomial {
    vi a;

    void norm() {
        if (a.size() > MAXM) {
            a.resize(MAXM);
        }
    }

    explicit polynomial(vi &&a) : a(a) {
        if ((*this).size() > MAXM) {
            (*this).a.resize(MAXM);
        }
    }

    explicit polynomial(const ll &val) : a(1, val) {
    }

    [[nodiscard]] size_t size() const {
        return a.size();
    }

    ll operator[](const size_t &index) const {
        if (index < a.size())
            return a[index];
        return 0;
    }

    void dumpWithoutDegree(const size_t &k) const {
        for (size_t i = 1; i <= k; ++i) {
            cout << (*this)[i] << '\n';
        }
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
    vi res(MAXM, 0);
    res[0] = p[0] / q[0];
    for (int i = 1; i < MAXM; ++i) {
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

    int k, n;
    cin >> k >> n;

    polynomial curP(move(vi{0, 1}));
    polynomial curQ(1);

    for (int i = 3; i <= k; i++) {
        polynomial newP = curQ;
        newP.a.insert(newP.a.begin(), 0);
        newP.norm();
        curQ = polynomial(-1) * curP + curQ;
        curP = newP;
    }

    (curP / curQ).dumpWithoutDegree(n);

    return 0;
}