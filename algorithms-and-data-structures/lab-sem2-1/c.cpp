#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

typedef long long ll;

struct treeNode {
    ll sum, pref, suff, ans;

    treeNode(const ll &sum, const ll &pref, const ll &suff, const ll &ans) : sum(sum), pref(pref), suff(suff),
                                                                             ans(ans) {};

    treeNode(const ll &val) {
        sum = val;
        pref = suff = ans = (val >= 0 ? val : 0);
    };

    treeNode() {
        sum = 0;
        pref = 0;
        suff = 0;
        ans = 0;
    };
};

vector<treeNode> t;

int lChild(const int &vertex) {
    return vertex * 2 + 1;
}

int rChild(const int &vertex) {
    return vertex * 2 + 2;
}

treeNode merge(const treeNode &a, const treeNode &b) {
    return treeNode(a.sum + b.sum,
                    max(a.pref, a.sum + b.pref),
                    max(b.suff, b.sum + a.suff),
                    max(max(a.ans, b.ans), a.suff + b.pref));
}

void treeBuild(const vector<ll> &a, const int &vertex, const int &tl, const int &tr) {
    if (tl + 1 == tr) {
        t[vertex] = treeNode(a[tl]);
        return;
    }

    int tm = (tl + tr) / 2;
    treeBuild(a, lChild(vertex), tl, tm);
    treeBuild(a, rChild(vertex), tm, tr);

    t[vertex] = merge(t[lChild(vertex)], t[rChild(vertex)]);
}

void set(const ll &index, const ll &val, const int &vertex, const int &tl, const int &tr) {
    if (tl + 1 == tr) {
        t[vertex] = treeNode(val);
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
    cout << t[0].ans << endl;
    for (int i = 0; i < m; ++i) {
        ll x, y;
        cin >> x >> y;
        set(x,y,0,0,n);
        cout << t[0].ans << endl;
    }

    return 0;
}