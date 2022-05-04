#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

typedef long long ll;

struct treeNode {
    ll mmin, set;

    treeNode(const ll &mmin, const ll &set) : mmin(mmin), set(set) {};

    treeNode() {
        mmin = INT64_MAX;
        set = -1;
    }
};

vector<treeNode> t;

template<typename T>
void dump(const vector<T> &arr) {
    for (int i = 0; i < arr.size(); i++) {
        cout << "i: " << i << " item: " << arr[i] << endl;
    }
    cout << endl;
}

void dump(const vector<treeNode> &arr) {
    for (int i = 0; i < arr.size(); i++) {
        cout << "i: " << i << " mmin: " << arr[i].mmin << " set: " << arr[i].set << endl;
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
    return treeNode(min(a.mmin, b.mmin), -1);
}

void treeBuild(const vector<ll> &a, const int &vertex, const int &tl, const int &tr) {
    if (tl + 1 == tr) {
        t[vertex] = treeNode(a[tl], -1);
        return;
    }

    int tm = (tl + tr) / 2;
    treeBuild(a, lChild(vertex), tl, tm);
    treeBuild(a, rChild(vertex), tm, tr);

    t[vertex] = merge(t[lChild(vertex)], t[rChild(vertex)]);
}

void propagate(const int &vertex) {
    if (t[vertex].set == -1) {
        return;
    }
    t[lChild(vertex)].mmin = t[vertex].set;
    t[rChild(vertex)].mmin = t[vertex].set;
    t[lChild(vertex)].set = t[vertex].set;
    t[rChild(vertex)].set = t[vertex].set;
    t[vertex].set = -1;
}

void set(const ll &val, const int &l, const int &r, const int &vertex, const int &tl, const int &tr) {
    if (r <= tl || tr <= l) {
        return;
    }

    if (l <= tl && tr <= r) {
        t[vertex].set = val;
        t[vertex].mmin = val;
        return;
    }

    propagate(vertex);

    int tm = (tl + tr) / 2;
    set(val, l, r, lChild(vertex), tl, tm);
    set(val, l, r, rChild(vertex), tm, tr);
    t[vertex] = merge(t[lChild(vertex)], t[rChild(vertex)]);
}

//void set(const ll &index, const ll &val, const int &vertex, const int &tl, const int &tr) {
//    if (tl + 1 == tr) {
//        t[vertex] = treeNode(val);
//        return;
//    }
//
//    int tm = (tl + tr) / 2;
//    if (index < tm) {
//        set(index, val, lChild(vertex), tl, tm);
//    } else {
//        set(index, val, rChild(vertex), tm, tr);
//    }
//
//    t[vertex] = merge(t[lChild(vertex)], t[rChild(vertex)]);
//}

treeNode query(const ll &l, const ll &r, const int &vertex, const int &tl, const int &tr) {
    if (l <= tl && tr <= r) {
        return t[vertex];
    }

    propagate(vertex);

    int tm = (tl + tr) / 2;
    if (r <= tm) {
        return query(l, r, lChild(vertex), tl, tm);
    }
    if (l >= tm)
        return query(l, r, rChild(vertex), tm, tr);
    return merge(
            query(l, r, lChild(vertex), tl, tm),
            query(l, r, rChild(vertex), tm, tr)
    );
}

int main() {
    ll n, m;
    cin >> n >> m;

    vector<ll> a(n, 0);

    t = vector<treeNode>(4 * n);
    treeBuild(a, 0, 0, n);

    for (int i = 0; i < m; ++i) {
        int func;
        cin >> func;
        if (func == 1) { // set
            ll l, r, v;
            cin >> l >> r >> v;
            set(v, l, r, 0, 0, n);
        } else if (func == 2) { // min
            ll l, r;
            cin >> l >> r;
            cout << query(l, r, 0, 0, n).mmin << endl;
        }
    }

    return 0;
}