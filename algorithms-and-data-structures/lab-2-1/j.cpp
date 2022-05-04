#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

typedef long long ll;

vector<ll> t;

int lChild(const int &vertex) {
    return vertex * 2 + 1;
}

int rChild(const int &vertex) {
    return vertex * 2 + 2;
}

void treeBuild(const vector<ll> &a, const int &vertex, const int &tl, const int &tr) {
    if (tl + 1 == tr) {
        t[vertex] = a[tl];
        return;
    }

    int tm = (tl + tr) / 2;
    treeBuild(a, lChild(vertex), tl, tm);
    treeBuild(a, rChild(vertex), tm, tr);

    t[vertex] = min(t[lChild(vertex)], t[rChild(vertex)]);
}

void set(const ll &index, const ll &h, const int &vertex, const int &tl, const int &tr) {
    if (tl + 1 == tr) {
        t[vertex] = h;
        return;
    }

    int tm = (tl + tr) / 2;
    if (index < tm) {
        set(index, h, lChild(vertex), tl, tm);
    } else {
        set(index, h, rChild(vertex), tm, tr);
    }

    t[vertex] = min(t[lChild(vertex)], t[rChild(vertex)]);
}

ll earthShake(const ll &l, const ll &r, const int &p, const int &vertex, const int &tl, const int &tr) {
    if (tl + 1 == tr) {
        if (l <= tl && tr <= r && p >= t[vertex]) {
            t[vertex] = INT64_MAX;
            return 1;
        } else {
            return 0;
        }
    }

    int tm = (tl + tr) / 2;
    ll r1 = 0, r2 = 0;
    if (p >= t[lChild(vertex)]) {
        r1 = earthShake(l, r, p, lChild(vertex), tl, tm);
    }
    if (p >= t[rChild(vertex)]) {
        r2 = earthShake(l, r, p, rChild(vertex), tm, tr);
    }
    if (r1 != 0 || r2 != 0) {
        t[vertex] = min(t[lChild(vertex)], t[rChild(vertex)]);
    }

    return r1 + r2;
}

int main() {
    ll n, m;
    cin >> n >> m;

    vector<ll> a(n, INT64_MAX);

    t = vector<ll>(4 * n);
    treeBuild(a, 0, 0, n);

    for (int i = 0; i < m; ++i) {
        ll func;
        cin >> func;
        if (func == 1) { // set
            ll id, h;
            cin >> id >> h;
            set(id, h, 0, 0, n);
        } else if (func == 2) { // earthShake
            ll l, r, p;
            cin >> l >> r >> p;
            cout << earthShake(l, r, p, 0, 0, n) << endl;
        }
    }

    return 0;
}