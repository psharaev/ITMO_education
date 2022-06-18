#include <stdio.h>
#include <algorithm>
#include <vector>

using namespace std;

struct treeNode {
    int x1, y1, x2, y2;
};

const int shiftCoor = 200000;
int n, res1, res2, res3;
vector<int> keeper, userer;

bool comp(const treeNode &a, const treeNode &b) {
    if (b.x2 < a.x2 && b.x1 == a.x1) {
        return true;
    } else {
        return b.x1 > a.x1;
    }
}

void propagate(const int &v, const int &l, const int &r) {
    int x = keeper[v];
    keeper[v] = 0;
    if (l == r) {
        return;
    }
    userer[2 * v] += x;
    userer[2 * v + 1] += x;
    keeper[2 * v] += x;
    keeper[2 * v + 1] += x;
}

void set(const int &v, const int &l, const int &r, const int &y1, const int &y2, const int &x2) {
    if (y1 > y2) {
        return;
    }
    if (l == y1 && y2 == r) {
        userer[v] += x2;
        keeper[v] += x2;
        return;
    }

    propagate(v, l, r);

    int m = (l + r) / 2;
    set(2 * v, l, m, y1, min(m, y2), x2);
    set(2 * v + 1, m + 1, r, max(m + 1, y1), y2, x2);
    userer[v] = max(userer[2 * v], userer[2 * v + 1]);
}

void query(const int &v, const int &l, const int &r, const int &x1) {
    if (l == r) {
        if (res1 < userer[v]) {
            res1 = userer[v];
            res2 = x1;
            res3 = l;
        }
        return;
    }
    propagate(v, l, r);
    int m = (l + r) / 2;
    if (userer[2 * v] > userer[2 * v + 1]) {
        query(2 * v, l, m, x1);
    } else {
        query(2 * v + 1, m + 1, r, x1);
    }
}

int main() {
    scanf("%i", &n);
    vector<treeNode> a = vector<treeNode>(2 * n);
    int maxCount = 4 * 4 * 4 * 100000;
    keeper = vector<int>(maxCount, 0);
    userer = vector<int>(maxCount, 0);
    for (int v = 0; v < n; v++) {
        int x1, x2, y1, y2;
        scanf("%i%i%i%i", &x1, &y1, &x2, &y2);
        a[2 * v].x1 = x1 + shiftCoor + 1;
        a[2 * v].x2 = 1;
        a[2 * v].y1 = y1 + shiftCoor + 1;
        a[2 * v].y2 = y2 + shiftCoor + 1;
        a[2 * v + 1].x1 = x2 + shiftCoor + 1;
        a[2 * v + 1].x2 = -1;
        a[2 * v + 1].y1 = y1 + shiftCoor + 1;
        a[2 * v + 1].y2 = y2 + shiftCoor + 1;
    }
    sort(a.begin(), a.end(), comp);
    res1 = -1;
    for (int i = 0; i < 2 * n; ++i) {
        set(1, 1, 2 * shiftCoor + 10, a[i].y1, a[i].y2, a[i].x2);
        query(1, 1, 2 * shiftCoor + 10, a[i].x1);
    }

    printf("%i\n%i %i", res1, res2 - shiftCoor - 1, res3 - shiftCoor - 1);
}