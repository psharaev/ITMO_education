#include <iostream>
#include <vector>

using namespace std;

typedef long long ll;

vector<ll> t;

template<typename T>
void dump(const vector<T> &arr) {
    for (int i = 0; i < arr.size(); i++) {
        cout << "i: " << i << " item: " << arr[i] << endl;
    }
    cout << endl;
}

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
    treeBuild(a, vertex * 2 + 1, tl, tm);
    treeBuild(a, vertex * 2 + 2, tm, tr);

    t[vertex] = t[vertex * 2 + 1] + t[vertex * 2 + 2];
}

void set(const ll &index, const ll &val, const int &vertex, const int &tl, const int &tr) {
    if (tl + 1 == tr) {
        t[vertex] = val;
        return;
    }

    int tm = (tl + tr) / 2;
    if (index < tm) {
        set(index, val, vertex * 2 + 1, tl, tm);
    } else {
        set(index, val, vertex * 2 + 2, tm, tr);
    }

    t[vertex] = t[vertex * 2 + 1] + t[vertex * 2 + 2];
}

ll sum(const int &vertex, const ll &l, const ll &r, const int &tl, const int &tr) {
    if (r <= tl || tr <= l) {
        return 0;
    }

    if (l <= tl && tr <= r) {
        return t[vertex];
    }

    int tm = (tl + tr) / 2;
    ll s1 = sum(vertex * 2 + 1, l, r, tl, tm);
    ll s2 = sum(vertex * 2 + 2, l, r, tm, tr);
    return s1 + s2;
}

int main() {
    ll n, m;
    cin >> n >> m;

    vector<ll> a(n);
    for (int i = 0; i < n; ++i) {
        cin >> a[i];
    }

    t = vector<ll>(4 * n, 0);
    treeBuild(a, 0, 0, n);
//    dump(t);

    for (int i = 0; i < m; ++i) {
        ll x, y, z;
        cin >> x >> y >> z;
        if (x == 1) { // set
            set(y, z, 0, 0, n);
        } else if (x == 2) { // sum
            cout << sum(0, y, z, 0, n) << endl;
        }
//        dump(t);
    }

    return 0;
}

