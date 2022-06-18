#include <vector>
#include <iostream>
#include <algorithm>

using namespace std;

typedef long long ll;
typedef vector<ll> vi;
const int M = 11;

struct polynomial;

polynomial operator*(const polynomial &p, const polynomial &q);

struct polynomial {
    vi a;

    vector<polynomial> pows = vector<polynomial>();

    explicit polynomial(vi &&a) : a(a) {
        while (this->a.size() > 1 && this->a.back() == 0)
            this->a.pop_back();
    }

    explicit polynomial(const ll &val) : a(1, val) {
    }

    static polynomial read(const size_t & degree) {
        vi res(degree + 1);
        for (size_t i = 0; i <= degree; ++i) {
            cin >> res[i];
        }
        return polynomial(move(res));
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

    polynomial &getPow(const size_t &p) {
        if (pows.empty()) {
            pows.emplace_back(vi(1, 1));
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

    [[nodiscard]] polynomial getInvert() const {
        vi c(M, 0LL);
        for (int i = 1; i < M; ++i) {
            c[i - 1] = (*this)[i] * i;
        }
        return polynomial(move(c));
    }

    polynomial &operator+=(const polynomial &o) {
        size_t n = size(), m = o.size();
        a.resize(max(n, m), 0);

        size_t min_size = min(n, m);

        for (size_t i = 0; i < min_size; ++i) {
            a[i] += o[i];
        }

        for (size_t i = n; i < m; ++i) {
            a[i] = o[i];
        }

        return *this;
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
        res[i] = p[i] + q[i];
    }

    return polynomial(move(res));
}

polynomial operator*(const polynomial &p, const polynomial &q) {
    size_t n = p.size(), m = q.size();
    vi res(n + m - 1, 0);

    for (size_t i = 0; i < n; ++i) {
        for (size_t j = 0; j < m; ++j) {
            res[i + j] = res[i + j] + p[i] * q[j];
        }
    }

    return polynomial(move(res));
}

polynomial operator/(const polynomial &p, const polynomial &q) {
    vi res(M, 0);
    res[0] = p[0] / q[0];
    for (int i = 1; i < M; ++i) {
        ll sum = 0;
        for (int j = 0; j < i; ++j) {
            sum = sum + res[j] * q[i - j];
        }
        res[i] = (p[i] - sum) / q[0];
    }
    return polynomial(move(res));
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    ll r;
    size_t d;
    cin >> r >> d;
    polynomial p = polynomial::read(d);
    polynomial quaz({1L, -r});
    polynomial R(r);
    vi fac(M, 1);
    for (int i = 1; i < M; i++) {
        fac[i] = fac[i - 1] * i;
    }
    vector<polynomial> pq;
    pq.emplace_back(polynomial(1L));
    for (int k = 1; k < M; ++k) {
        polynomial curP = pq[k - 1].getInvert() * quaz + pq[k - 1] * R * polynomial(k);
        polynomial res(0);
        for (size_t j = 0; j < k; ++j) {
            res += R * polynomial(fac[k] / (fac[j] * fac[k - j])) * quaz.getPow(k - j) * pq[j];
        }
        pq.emplace_back((curP + polynomial(-1) * res) / R);
    }
    polynomial P(0);
    for (size_t i = 0; i <= d; ++i) {
        P += polynomial(p[i]) * quaz.getPow(d - i) * pq[i];
    }
    P.dumpWithDegree();
    quaz.getPow(d + 1).dumpWithDegree();
}
/*
2
0
1

3
3
2 3 9 1
*/