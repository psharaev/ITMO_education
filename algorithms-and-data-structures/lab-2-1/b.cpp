#include <iostream>
#include <vector>

using namespace std;

typedef long long ll;

struct treeNode {
    ll mmin, count;

    treeNode(const ll &mmin, const ll &count) : mmin(mmin), count(count) {};

    treeNode() {
        mmin = INT64_MAX;
        count = 0;
    }
};

treeNode neutralItem;
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
    if (a.mmin < b.mmin) {
        return a;
    } else if (a.mmin == b.mmin) {
        return treeNode(a.mmin, a.count + b.count);
    } else {
        return b;
    }
}

void treeBuild(const vector<ll> &a, const int &vertex, const int &tl, const int &tr) {
    if (tl + 1 == tr) {
        t[vertex] = treeNode(a[tl], 1);
        return;
    }

    int tm = (tl + tr) / 2;
    treeBuild(a, lChild(vertex), tl, tm);
    treeBuild(a, rChild(vertex), tm, tr);

    t[vertex] = merge(t[lChild(vertex)], t[rChild(vertex)]);
}

void set(const ll &index, const ll &val, const int &vertex, const int &tl, const int &tr) {
    if (tl + 1 == tr) {
        t[vertex] = treeNode(val, 1);
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

treeNode requery(const int &vertex, const ll &l, const ll &r, const int &tl, const int &tr) {
    if (r <= tl || tr <= l) {
        return neutralItem;
    }

    if (l <= tl && tr <= r) {
        return t[vertex];
    }

    int tm = (tl + tr) / 2;
    treeNode r1 = requery(lChild(vertex), l, r, tl, tm);
    treeNode r2 = requery(rChild(vertex), l, r, tm, tr);
    return merge(r1, r2);
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
//    dump(t);

    for (int i = 0; i < m; ++i) {
        ll x, y, z;
        cin >> x >> y >> z;
        if (x == 1) { // set
            set(y, z, 0, 0, n);
        } else if (x == 2) { // sum
            treeNode res = requery(0, y, z, 0, n);
            cout << res.mmin << ' ' << res.count << endl;
        }
//        dump(t);
    }

    return 0;
}

