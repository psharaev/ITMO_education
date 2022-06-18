#include <iostream>
#include <vector>
#include <numeric>
#include <algorithm>

using namespace std;

typedef long long ll;
typedef vector<ll> vi;

const int MAXM = 20;

struct fraction {
    ll p, q;

    explicit fraction(const ll &p, const ll &q) {
        ll t = gcd(p, q);
        this->p = p / t;
        this->q = q / t;
    }

    explicit fraction(const ll &p) : p(p), q(1) {}

    explicit fraction() : p(0), q(1) {}

    fraction &operator+=(const fraction &o) {
        ll _lcm = lcm(q, o.q);

        p = p * (_lcm / q) + o.p * (_lcm / o.q);
        q = _lcm;
        ll t = gcd(p, q);
        p /= t;
        q /= t;
        return *this;
    }
};

fraction operator+(const fraction &a, const fraction &b) {
    ll _lcm = lcm(a.q, b.q);
    return fraction(a.p * (_lcm / a.q) + b.p * (_lcm / b.q), _lcm);
}

fraction operator-(const fraction &a, const fraction &b) {
    return a + fraction(-b.p, b.q);
}

fraction operator*(const fraction &a, const fraction &b) {
    return fraction(a.p * b.p, a.q * b.q);
}

fraction operator/(const fraction &a, const fraction &b) {
    return fraction(a.p * b.q, a.q * b.p);
}

ostream &operator<<(ostream &os, const fraction &a) {
    return os << a.p << '/' << a.q;
}

typedef vector<fraction> vf;

struct polynomial {
    vector<fraction> a;

    explicit polynomial(vector<fraction> &&a) : a(a) {
        while (this->a.size() > MAXM) {
            this->a.pop_back();
        }
    }

    explicit polynomial(const fraction &val) : a(1, val) {
    }



    [[nodiscard]] size_t size() const {
        return a.size();
    }

    fraction operator[](const size_t &index) const {
        if (index < a.size())
            return a[index];
        return fraction(0);
    }

    void dumpWithoutDegree(const size_t &k) const {
        for (int i = 0; i <= k; i++) {
            cout << a[i] << ' ';
        }
        cout << '\n';
    }

    polynomial &operator+=(const polynomial &o) {
        size_t n = size(), m = o.size();
        a.resize(max(n, m), fraction(0));

        size_t min_size = min(n, m);

        for (size_t i = 0; i < min_size; ++i) {
            a[i] += o[i];
        }

        for (size_t i = n; i < m; ++i) {
            a[i] = o[i];
        }

        return *this;
    }

    polynomial &operator*=(const polynomial &o) {
        size_t n = size(), m = o.size();
        vf res(n + m - 1, fraction(0));

        for (size_t i = 0; i < n; ++i) {
            for (size_t j = 0; j < m; ++j) {
                res[i + j] += (*this)[i] * o[j];
            }
        }

        a = move(res);

        return *this;
    }
};

polynomial operator+(const polynomial &p, const polynomial &q) {
    size_t n = p.size(), m = q.size();
    vf res;

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
    vf res(n + m - 1, fraction(0));

    for (size_t i = 0; i < n; ++i) {
        for (size_t j = 0; j < m; ++j) {
            res[i + j] += p[i] * q[j];
        }
    }

    return polynomial(move(res));
}

polynomial operator/(const polynomial &p, const polynomial &q) {
    vf res(MAXM, fraction(0));
    res[0] = p[0] / q[0];
    for (int i = 1; i < MAXM; ++i) {
        fraction sum(0);
        for (int j = 0; j < i; ++j) {
            sum += res[j] * q[i - j];
        }
        res[i] = (p[i] - sum) / q[0];
    }
    return polynomial(move(res));
}


int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int r, k;
    cin >> r >> k;

    vf p;
    for (int i = 0; i <= k; ++i) {
        int t;
        cin >> t;
        p.emplace_back(t, 1);
    }

    vi fac(k + 1, 1);

    for (int i = 1; i <= k; i++) {
        fac[i] = fac[i - 1] * i;
    }

    polynomial res(fraction(0));
    for (int i = 0; i <= k; ++i) {
        polynomial c(fraction(1));
        for (ll j = k; j > 0; --j) {
            c *= polynomial(move(vector<fraction>{fraction(j - i), fraction(1)}));
        }
        c *= polynomial(fraction(1, fac[k]));
        for (int j = 0; j < i; ++j) {
            c = c / polynomial(fraction(r));
        }
        res += c * polynomial(p[i]);
    }
    res.dumpWithoutDegree(k);
}