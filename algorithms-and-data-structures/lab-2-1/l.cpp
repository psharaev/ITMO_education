#include <stdio.h>
#include <algorithm>
#include <vector>

using namespace std;

typedef long long ll;

struct TreeNode_t {
    ll sum, sSum, add, l, count;
};

vector<TreeNode_t> tree;

ll getSSum(const TreeNode_t &d) {
    return (2 * d.l + d.count - 1) * d.count / 2;
}

void propagate(const int &v) {
    tree[2 * v + 1].add += tree[v].add;
    tree[2 * v + 2].add += tree[v].add;
    tree[2 * v + 1].sum += tree[v].add * tree[2 * v + 1].count;
    tree[2 * v + 2].sum += tree[v].add * tree[2 * v + 2].count;
    tree[2 * v + 1].sSum += tree[v].add * getSSum(tree[2 * v + 1]);
    tree[2 * v + 2].sSum += tree[v].add * getSSum(tree[2 * v + 2]);
    tree[v].add = 0;
}

void add(const int &v, const int &lv, const int &rv, const int &l, const int &r, const int &d) {
    if (lv >= r || rv <= l) {
        return;
    }

    if (lv >= l && rv <= r) {
        tree[v].sum += d * tree[v].count;
        tree[v].sSum += d * getSSum(tree[v]);
        tree[v].add += d;
        return;
    }

    propagate(v);

    int m = (lv + rv) / 2;
    add(2 * v + 1, lv, m, l, r, d);
    add(2 * v + 2, m, rv, l, r, d);
    tree[v].sum = tree[2 * v + 1].sum + tree[2 * v + 2].sum;
    tree[v].sSum = tree[2 * v + 1].sSum + tree[2 * v + 2].sSum;
}

ll search(const int &v, const int &lv, const int &rv, const int &l, const int &r, const ll &val) {
    if (lv >= r || rv <= l) {
        return 0;
    }
    if (lv >= l && rv <= r) {
        return (tree[v].sSum + val * getSSum(tree[v])) - l * (tree[v].sum + val * tree[v].count);
    }

    int m = (lv + rv) / 2;
    return search(2 * v + 1, lv, m, l, r, val + tree[v].add) +
           search(2 * v + 2, m, rv, l, r, val + tree[v].add);
}

void treeBuild(const vector<TreeNode_t> &a, const int &vertex, const int &tl, const int &tr) {
    if (tl + 1 == tr) {
        tree[vertex] = a[tl];
        return;
    }

    int tm = (tl + tr) / 2;
    treeBuild(a, 2 * vertex + 1, tl, tm);
    treeBuild(a, 2 * vertex + 2, tm, tr);

    tree[vertex].sum = tree[2 * vertex + 1].sum + tree[2 * vertex + 2].sum;
    tree[vertex].sSum = tree[2 * vertex + 1].sSum + tree[2 * vertex + 2].sSum;
    tree[vertex].l = tree[2 * vertex + 1].l;
    tree[vertex].count = tree[2 * vertex + 1].count + tree[2 * vertex + 2].count;
}

int main() {
    int n, m;
    scanf("%i%i", &n, &m);
    tree = vector<TreeNode_t>(4 * n);
    vector<TreeNode_t> a = vector<TreeNode_t>(n);

    for (int i = 0; i < n; i++) {
        scanf("%lli", &a[i].sum);
        a[i].sSum = a[i].sum * (i + 1);
        a[i].add = 0;
        a[i].l = i + 1;
        a[i].count = 1;
    }
    treeBuild(a, 0, 0, n);

    for (int i = 0; i < m; i++) {
        int t, l, r;
        scanf("%i%i%i", &t, &l, &r);
        l--;
        
        if (t == 1) {
            int d;
            scanf("%i", &d);
            add(0, 0, n, l, r, d);
        } else if (t == 2) {
            ll sum = search(0, 0, n, l, r, 0);
            printf("%lli\n", sum);
        }
    }

    return 0;
}
