#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

typedef long long ll;

#define W 0
#define B 1

const int shift = 500000;

// 0 - false - белый
// 1 - true - чёрный
struct treeNode {
    int count, sum;
    bool l, r;
    int paint; // 0 - W; 1 - B; 2 - None

    treeNode(const int &count, const int &sum, const bool &l, const bool &r, const int &paint)
            : count(count), sum(sum), l(l), r(r), paint(paint) {}

    treeNode(const bool &col, const int &dict) {
        if (col) { // B
            count = 1;
            sum = dict;
            l = r = true;
            paint = B;
        } else { // W
            count = 0;
            sum = 0;
            l = r = false;
            paint = W;
        }
    }

    treeNode() {
        count = sum = 0;
        l = false;
        r = false;
        paint = 2;
    }
};

vector<treeNode> t;

void dump(const vector<treeNode> &t) {
    for (int i = 0; i < t.size(); i++) {
        cout << "i: " << i << " count: " << t[i].count << " sum: " << t[i].sum << " l: " << t[i].l << " r: " << t[i].r
             << endl;
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
    return treeNode(a.count + b.count + (a.r && b.l ? -1 : 0), a.sum + b.sum, a.l, b.r,
                    a.paint == b.paint ? a.paint : 2);
}

void propagate(const int &vertex, const int &tl, const int &tm, const int &tr) {
    if (t[vertex].paint == 1) { // целиком чёрный отрезок
        t[lChild(vertex)].count = 1;
        t[lChild(vertex)].sum = tm - tl;
        t[lChild(vertex)].l = true;
        t[lChild(vertex)].r = true;
        t[lChild(vertex)].paint = B;

        t[rChild(vertex)].count = 1;
        t[rChild(vertex)].sum = tr - tm;
        t[rChild(vertex)].l = true;
        t[rChild(vertex)].r = true;
        t[rChild(vertex)].paint = B;
        t[vertex].paint = 2;
    } else if (t[vertex].paint == 0) { // целиком белый отрезок
        t[lChild(vertex)].count = 0;
        t[lChild(vertex)].sum = 0;
        t[lChild(vertex)].l = false;
        t[lChild(vertex)].r = false;
        t[lChild(vertex)].paint = W;

        t[rChild(vertex)].count = 0;
        t[rChild(vertex)].sum = 0;
        t[rChild(vertex)].l = false;
        t[rChild(vertex)].r = false;
        t[rChild(vertex)].paint = W;
        t[vertex].paint = 2;
    }
}

void set(const int &l, const int &r, const bool &col, const int &vertex, const int &tl, const int &tr) {
    if (r <= tl || tr <= l) {
        return;
    }

    if (l <= tl && tr <= r) {
        if (col) { // black
            t[vertex] = treeNode(B, tr - tl);
        } else { // white
            t[vertex] = treeNode(W, 0);
        }
        return;
    }

    int tm = (tl + tr) / 2;
    propagate(vertex, tl, tm, tr);
    set(l, r, col, lChild(vertex), tl, tm);
    set(l, r, col, rChild(vertex), tm, tr);
    t[vertex] = merge(t[lChild(vertex)], t[rChild(vertex)]);
}

int main() {
    int n;
    cin >> n;

    t = vector<treeNode>(4 * 1000002, treeNode());

    t[0].paint = 0;
    for (int i = 0; i < n; ++i) {
        char c;
        int s, len;
        cin >> c >> s >> len;
        s += shift;

        set(s, s + len, c == 'B', 0, 0, 1000002);

        cout << t[0].count << ' ' << t[0].sum << endl;
    }

    return 0;
}