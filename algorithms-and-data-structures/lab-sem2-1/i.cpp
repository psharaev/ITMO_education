#include <vector>
#include <cstdio>

using namespace std;

typedef long long ll;

ll r;

struct matrix_t {
    ll a11, a12, a21, a22;

    matrix_t(const ll &a11, const ll &a12, const ll &a21, const ll &a22) : a11(a11), a12(a12), a21(a21), a22(a22) {};

    matrix_t(const matrix_t &b) {
        a11 = b.a11;
        a12 = b.a12;
        a21 = b.a21;
        a22 = b.a22;
    }

    matrix_t() {
        a11 = a12 = a21 = a22 = 0;
    };

    matrix_t operator*(const matrix_t &b) const {
        return matrix_t((this->a11 * b.a11 + this->a12 * b.a21) % r,
                        (this->a11 * b.a12 + this->a12 * b.a22) % r,
                        (this->a21 * b.a11 + this->a22 * b.a21) % r,
                        (this->a21 * b.a12 + this->a22 * b.a22) % r);
    }
};

vector<matrix_t> t;

void treeBuild(const vector<matrix_t> &a, const int &vertex, const int &tl, const int &tr) {
    if (tl + 1 == tr) {
        t[vertex] = a[tl];
        return;
    }

    int tm = (tl + tr) / 2;
    treeBuild(a, vertex * 2 + 1, tl, tm);
    treeBuild(a, vertex * 2 + 2, tm, tr);

    t[vertex] = t[(vertex * 2 + 1)] * t[(vertex * 2 + 2)];
}

matrix_t get(const int &a, const int &b, const int &vertex, const int &tl, const int &tr) {
    if (a <= tl && tr <= b) {
        return t[vertex];
    }

    int tm = (tl + tr) / 2;
    if (b <= tm) {
        return get(a, b, vertex * 2 + 1, tl, tm);
    }
    if (a >= tm) {
        return get(a, b, vertex * 2 + 2, tm, tr);
    }
    return get(a, b, vertex * 2 + 1, tl, tm) * get(a, b, vertex * 2 + 2, tm, tr);
}

int main() {
    int n, m;
    scanf("%lld%d%d", &r, &n, &m);

    vector<matrix_t> a(n);
    for (int i = 0; i < n; ++i) {
        ll a11, a12, a21, a22;
        scanf("%lld%lld%lld%lld", &a11, &a12, &a21, &a22);
        a[i] = matrix_t(a11, a12, a21, a22);
    }

    t = vector<matrix_t>(4 * n);
    treeBuild(a, 0, 0, n);

    for (int i = 0; i < m; ++i) {
        int x, y;
        scanf("%d%d", &x, &y);
        matrix_t res = get(x - 1, y, 0, 0, n);
        printf("%lld %lld\n%lld %lld\n\n", res.a11, res.a12, res.a21, res.a22);
    }

    return 0;
}