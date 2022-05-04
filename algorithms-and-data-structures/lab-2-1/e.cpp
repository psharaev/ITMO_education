#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

typedef long long ll;

struct treeNode {
    ll mmax, id;

    treeNode(const ll &mmax, const ll &id) : mmax(mmax), id(id) {};

    treeNode() {
        mmax = INT64_MIN;
        id = -1;
    };
};

vector<treeNode> t;

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

treeNode merge(const treeNode &a, const treeNode &b) {
    return treeNode(max(a.mmax, b.mmax), (a.mmax >= b.mmax ? a.id : b.id));
}

void treeBuild(const vector<ll> &a, const int &vertex, const int &tl, const int &tr) {
    if (tl + 1 == tr) {
        t[vertex] = treeNode(a[tl], tl);
        return;
    }

    int tm = (tl + tr) / 2;
    treeBuild(a, lChild(vertex), tl, tm);
    treeBuild(a, rChild(vertex), tm, tr);

    t[vertex] = merge(t[lChild(vertex)], t[rChild(vertex)]);
}

void set(const ll &index, const ll &val, const int &vertex, const int &tl, const int &tr) {
    if (tl + 1 == tr) {
        t[vertex] = treeNode(val, tl);
        return;
    }

    int tm = (tl + tr) / 2;
    if (index < tm) {
        set(index, val, lChild(vertex), tl, tm);
    } else {
        set(index, val, rChild(vertex), tm, tr);
    }

    t[vertex] = merge(t[lChild(vertex)], t[rChild(vertex)]);
}

ll find(const ll &x, const ll &l, const int &vertex, const int &tl, const int &tr) {
    if (tl + 1 == tr) {
        if (l <= tl && x <= t[vertex].mmax) {
            return tl;
        } else {
            return -1;
        }
    }

    int tm = (tl + tr) / 2;
    ll r1 = -1, r2 = -1;
    if (l < tm && x <= t[lChild(vertex)].mmax) {
        r1 = find(x, l, lChild(vertex), tl, tm);
    }
    if (r1 != -1) {
        return r1;
    }

    if (l < tr && x <= t[rChild(vertex)].mmax) {
        r2 = find(x, l, rChild(vertex), tm, tr);
    }
    return r2;
}

int main() {
    ll n, m;
    cin >> n >> m;

    vector<ll> a(n);
    for (int i = 0; i < n; ++i) {
        cin >> a[i];
    }

    t = vector<treeNode>(4 * n);
    treeBuild(a, 0, 0, n);

    for (int i = 0; i < m; ++i) {
        ll func;
        cin >> func;
        if (func == 1) { // set
            ll id, v;
            cin >> id >> v;
            set(id, v, 0, 0, n);
        } else if (func == 2) { // find
            ll x, l;
            cin >> x >> l;
            cout << find(x, l, 0, 0, n) << endl;
        }
    }

    return 0;
}