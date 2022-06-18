#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;

vvi g;
vi mt;
vb used;

bool try_kuhn(const int &v) {
    if (used[v]) {
        return false;
    }
    used[v] = true;
    for (const auto &to: g[v]) {
        if (mt[to] == -1 || try_kuhn(mt[to])) {
            mt[to] = v;
            return true;
        }
    }
    return false;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n, m, a, b;
    cin >> n >> m >> a >> b;

    g.resize(n * m, vi());
    mt.assign(n * m, -1);
    vector<vector<char>> s(n, vector<char>(m));
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            cin >> s[i][j];
        }
    }

    int _empty = 0;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (s[i][j] == '.') {
                continue;
            }
            _empty++;
            if ((i + j) % 2 != 0) {
                continue;
            }
            int u = i * m + j;
            if (j > 0 && (s[i][j - 1] == '*')) {
                g[u].push_back(u - 1);
            }
            if (i > 0 && (s[i - 1][j] == '*')) {
                g[u].push_back(u - m);
            }
            if ((j < m - 1) && (s[i][j + 1] == '*')) {
                g[u].push_back(u + 1);
            }
            if ((i < n - 1) && (s[i + 1][j] == '*')) {
                g[u].push_back(u + m);
            }
        }
    }

    if (2 * b <= a) {
        cout << _empty * b;
        return 0;
    }

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if ((i + j) % 2 != 0) {
                continue;
            }
            used.assign(n * m, false);
            try_kuhn(i * m + j);
        }
    }

    int countInMT = 0;
    for (const auto &par: mt) {
        if (par != -1) {
            countInMT++;
        }
    }

    cout << countInMT * a + (_empty - 2 * countInMT) * b;
    return 0;
}