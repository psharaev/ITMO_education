#include <iostream>
#include <cstdio>
#include <algorithm>
#include <vector>

using namespace std;

typedef long long ll;

struct treeNode {
    ll set, add, sum;

    treeNode(const ll &set, const ll &add, const ll &sum) : set(set), add(add), sum(sum) {};

    treeNode() {
        set = -1;
        add = 0LL;
        sum = 0LL;
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
        cout << "i: " << i << " set: " << arr[i].set << " add: " << arr[i].add << endl;
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
    return treeNode(-1, 0, a.sum + b.sum);
}

void treeBuild(const vector<ll> &a, const int &vertex, const int &tl, const int &tr) {
    if (tl + 1 == tr) {
        t[vertex] = treeNode(-1, 0, a[tl]);
        return;
    }

    int tm = (tl + tr) / 2;
    treeBuild(a, lChild(vertex), tl, tm);
    treeBuild(a, rChild(vertex), tm, tr);

    t[vertex] = merge(t[lChild(vertex)], t[rChild(vertex)]);
}

void propagate(const int &vertex, const int &tl, const int &tm, const int &tr) {
    if (t[vertex].add != 0) { // если надо протолкнуть +
        if (t[lChild(vertex)].set != -1) { // есть ли у ребёнка проталкивание =
            t[lChild(vertex)].set += t[vertex].add;
        } else {
            t[lChild(vertex)].add += t[vertex].add;
        }
        t[lChild(vertex)].sum += (tm - tl) * t[vertex].add;

        if (t[rChild(vertex)].set != -1) { // есть ли у ребёнка проталкивание =
            t[rChild(vertex)].set += t[vertex].add;
        } else {
            t[rChild(vertex)].add += t[vertex].add;
        }
        t[rChild(vertex)].sum += (tr - tm) * t[vertex].add;

        t[vertex].add = 0;
    } else if (t[vertex].set != -1) { // если надо протолкнуть =
        t[lChild(vertex)].set = t[vertex].set;
        t[lChild(vertex)].add = 0;
        t[lChild(vertex)].sum = (tm - tl) * t[vertex].set;

        t[rChild(vertex)].set = t[vertex].set;
        t[rChild(vertex)].add = 0;
        t[rChild(vertex)].sum = (tr - tm) * t[vertex].set;

        t[vertex].set = -1;
    }
}

void add(const int &l, const int &r, const ll &val, const int &vertex, const int &tl, const int &tr) {
    if (r <= tl || tr <= l) {
        return;
    }

    if (l <= tl && tr <= r) {
        if (t[vertex].set == -1) {
            t[vertex].add += val;
        } else {
            t[vertex].set += val;
        }
        t[vertex].sum += (tr - tl) * val;
        return;
    }


    int tm = (tl + tr) / 2;

    propagate(vertex, tl, tm, tr);

    add(l, r, val, lChild(vertex), tl, tm);
    add(l, r, val, rChild(vertex), tm, tr);
    t[vertex] = merge(t[lChild(vertex)], t[rChild(vertex)]);
}

void set(const int &l, const int &r, const ll &val, const int &vertex, const int &tl, const int &tr) {
    if (r <= tl || tr <= l) {
        return;
    }

    if (l <= tl && tr <= r) {
        t[vertex].add = 0;
        t[vertex].set = val;
        t[vertex].sum = (tr - tl) * val;
        return;
    }

    int tm = (tl + tr) / 2;

    propagate(vertex, tl, tm, tr);

    set(l, r, val, lChild(vertex), tl, tm);
    set(l, r, val, rChild(vertex), tm, tr);
    t[vertex] = merge(t[lChild(vertex)], t[rChild(vertex)]);
}

treeNode sum(const ll &l, const ll &r, const int &vertex, const int &tl, const int &tr) {
    if (l <= tl && tr <= r) {
        return t[vertex];
    }

    int tm = (tl + tr) / 2;

    propagate(vertex, tl, tm, tr);

    if (r <= tm) {
        return sum(l, r, lChild(vertex), tl, tm);
    }
    if (l >= tm)
        return sum(l, r, rChild(vertex), tm, tr);
    return merge(
            sum(l, r, lChild(vertex), tl, tm),
            sum(l, r, rChild(vertex), tm, tr)
    );
}

int main() {
    int n, m;
    scanf("%d%d", &n, &m);

    vector<ll> a(n, 0);

    t = vector<treeNode>(4 * n);
    treeBuild(a, 0, 0, n);

    for (int i = 0; i < m; ++i) {
        int func, l, r;
        scanf("%d%d%d", &func, &l, &r);

        if (func == 1) { // set
            ll v;
            scanf("%lld", &v);
//            printf("start set\n");
            set(l, r, v, 0, 0, n);
//            printf("end set\n");
        } else if (func == 2) { // add
            ll v;
            scanf("%lld", &v);
            if (v == 0) {
                continue;
            }
//            printf("start add\n");
            add(l, r, v, 0, 0, n);
//            printf("end add\n");
        } else if (func == 3) { // sum
//            printf("start sum\n");
            treeNode res = sum(l, r, 0, 0, n);
//            printf("end sum\n");
            printf("%lld\n", res.sum);
        }
    }

    return 0;
}