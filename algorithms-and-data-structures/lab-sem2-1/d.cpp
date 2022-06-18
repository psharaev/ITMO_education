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
    treeBuild(a, lChild(vertex), tl, tm);
    treeBuild(a, rChild(vertex), tm, tr);

    t[vertex] = t[lChild(vertex)] + t[rChild(vertex)];
}

void invert(const ll &index, const int &vertex, const int &tl, const int &tr) {
    if (tl + 1 == tr) {
        t[vertex] = 1 - t[vertex];
        return;
    }

    int tm = (tl + tr) / 2;
    if (index < tm) {
        invert(index, lChild(vertex), tl, tm);
    } else {
        invert(index, rChild(vertex), tm, tr);
    }

    t[vertex] = t[lChild(vertex)] + t[rChild(vertex)];
}

//void set(const ll &index, const ll &val, const int &vertex, const int &tl, const int &tr) {
//    if (tl + 1 == tr) {
//        t[vertex] = treeNode(val, 1);
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

int requery(const int &vertex, const ll &k, const int &tl, const int &tr) {
    if (tl + 1 == tr) {
        return tl;
    }

    int tm = (tl + tr) / 2;
    if (t[lChild(vertex)] >= k) {
        return requery(lChild(vertex), k, tl, tm);
    } else {
        return requery(rChild(vertex), k - t[lChild(vertex)], tm, tr);
    }
}

int main() {
    ll n, m;
    cin >> n >> m;

    vector<ll> a(n);
    for (int i = 0; i < n; ++i) {
        cin >> a[i];
    }

    t = vector<ll>(4 * n);
    treeBuild(a, 0, 0, n);

    for (int i = 0; i < m; ++i) {
        ll x, y;
        cin >> x >> y;
        if (x == 1) { // invert
            invert(y, 0, 0, n);
        } else if (x == 2) { // k
            cout << requery(0, y + 1, 0, n) << endl;
        }
    }

    return 0;
}

