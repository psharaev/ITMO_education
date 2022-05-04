#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef vector<vb> vvb;

inline void findNextLeaf(const int &n, const vi &d, int& ptr) {
    while (ptr < n && d[ptr] != 1) {
        ++ptr;
    }
}

inline void dump(const int &a, const int &b) {
    cout << a + 1 << ' ' << b + 1 << endl;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n;
    cin >> n;

    vi p(n - 2);
    for (int i = 0; i < n - 2; ++i) {
        cin >> p[i];
        p[i]--;
    }

    vi d(n, 1);
    for (int i = 0; i < n - 2; ++i) {
        ++d[p[i]];
    }

    int ptr = 0;
    findNextLeaf(n, d, ptr);
    int l = ptr;

    for (int i = 0; i < n - 2; ++i) {
        int v = p[i];
        dump(l, v);

        --d[l];
        if (--d[v] == 1 && v < ptr) {
            l = v;
        } else {
            ++ptr;
            findNextLeaf(n, d, ptr);
            l = ptr;
        }
    }

    for (int v = 0; v < n - 1; ++v) {
        if (d[v] == 1) {
            dump(v, n - 1);
        }
    }

    return 0;
}
