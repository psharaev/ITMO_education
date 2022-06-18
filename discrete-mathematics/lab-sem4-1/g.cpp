#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

typedef long long ll;
typedef vector<ll> vi;

const int MAXM = 7;

struct polynomial;

polynomial operator*(const polynomial &p, const polynomial &q);

struct polynomial {
    vector<ll> a;

    explicit polynomial(vector<ll> &&a) : a(a) {
        if (a.size() > MAXM) {
            a.resize(MAXM);
        }
    }

    explicit polynomial(const ll &val) : a(1, val) {
    }

    [[nodiscard]] size_t size() const {
        return a.size();
    }

    [[nodiscard]] size_t degree() const {
        return a.size() - 1;
    }

    polynomial getPow(size_t n) {
        polynomial res(1);
        while (n) {
            if ((n & 1) == 1) {
                res = res * (*this);
                n--;
            } else {
                (*this) = (*this) * (*this);
                n /= 2;
            }
        }
        return res;
    }

    ll operator[](const size_t &index) const {
        if (index < a.size())
            return a[index];
        return 0;
    }

    void dumpWithoutDegree(const size_t &k) const {
        for (size_t i = 0; i < k; ++i) {
            cout << (*this)[i] << ' ';
        }
        cout << '\n';
    }
};

const polynomial one(1);
const polynomial none(-1);

polynomial operator+(const polynomial &p, const polynomial &q) {
    vi res(MAXM);

    for (size_t i = 0; i < MAXM; ++i) {
        res[i] = p[i] + q[i];
    }

    return polynomial(move(res));
}

polynomial operator*(const polynomial &p, const polynomial &q) {
    vi res(2 * MAXM, 0);

    for (size_t i = 0; i < MAXM; ++i) {
        for (size_t j = 0; j < MAXM; ++j) {
            res[i + j] += p[i] * q[j];
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
            sum += res[j] * q[i - j];
        }
        res[i] = (p[i] - sum) / q[0];
    }
    return polynomial(move(res));
}

polynomial getSet(const polynomial &a) {
    polynomial res(1);
    for (int i = 1; i <= MAXM; ++i) {
        res = res * (one / (one + none * polynomial(vi{0, 1}).getPow(i)).getPow(a[i]));
    }
    return res;
}

polynomial parse() {
    char c;
    cin >> c;
    switch (c) {
        case 'P': {
            cin >> c;
            polynomial a = parse();
            cin >> c;
            polynomial b = parse();
            cin >> c;
            return a * b;
        }
        case 'L': {
            cin >> c;
            polynomial a = parse();
            a.a[0] = 0;
            cin >> c;
            return one / (one + none * a);
        }
        case 'B': {
            return polynomial(vi{0, 1});
        }
        case 'S' : {
            cin >> c;
            polynomial a = parse();
            cin >> c;
            return getSet(a);
        }
        default:
            return polynomial(0);
    }
}


int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    parse().dumpWithoutDegree(MAXM);
}