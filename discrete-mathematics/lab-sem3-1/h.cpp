#include <iostream>
#include <vector>
#include <set>
#include <algorithm>

using namespace std;

typedef long long ll;

typedef set<int> si;
typedef vector<si> vsi;
typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef vector<vb> vvb;

typedef vvb graph_t;

int d;

vvb cut(const vvb &g, int u, int v) {
    vvb res(g);
    res[u][v] = res[v][u] = false;
    return res;
}

vvb constriction(const vvb &g, int u, int v) {
    int n = (int) g.size();
    if (u > v) {
        swap(u, v);
    }
    vvb res(n - 1, vb(n - 1));
    for (int i = 0; i < n; ++i) {
        if (i == v) {
            continue;
        }

        int i_res = (i < v ? i : i - 1);

        copy(g[i].begin(), g[i].begin() + v, res[i_res].begin());
        copy(g[i].begin() + v + 1, g[i].end(), res[i_res].begin() + v);
    }

    for (int i = 0; i < n; ++i) {
        if (i == v) {
            continue;
        }
        int i_res = (i < v ? i : i - 1);
        res[u][i_res] = res[u][i_res] || g[v][i];
    }

    for (int i = 0; i < n; ++i) {
        if (i == v) {
            continue;
        }
        int i_res = (i < v ? i : i - 1);
        res[i_res][u] = res[i_res][u] || g[i][v];
    }

    res[u][u] = false;

    return res;
}

bool isZeroG(const vvb &g) {
    int n = (int) g.size();
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            if (g[i][j]) {
                return false;
            }
        }
    }
    return true;
}

vi sub(const vi &a, const vi &b) {
    vi res(d);
    for (int i = 0; i < d; ++i) {
        res[i] = a[i] - b[i];
    }
    return res;
}

pair<int, int> findEdge(const vvb &g) {
    int n = (int) g.size();
    for (int i = 1; i < n; ++i) {
        for (int j = 0; j < i; ++j) {
            if (g[i][j]) {
                return {i, j};
            }
        }
    }
}

vi P(const vvb &g) {
    vi cp(d, 0);
    if (isZeroG(g)) {
        int deg = g.size();
        cp[deg]++;
        return cp;
    }

    pair<int, int> uv = findEdge(g);
    int u = uv.first, v = uv.second;

    return sub(P(cut(g, u, v)),
                    P(constriction(g, u, v)));
}

void dump(const vvb &g) {
    for (const auto &row: g) {
        for (const auto &i: row) {
            cout << i << ' ';
        }
        cout << endl;
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n, m;
    cin >> n >> m;
    d = n + 1;

    graph_t g(n, vb(n, false));
    vi c(n, -1);

    for (int i = 0; i < m; ++i) {
        int a, b;
        cin >> a >> b;
        a--;
        b--;
        g[a][b] = g[b][a] = true;
    }

    vi cp = move(P(g));

    reverse(cp.begin(), cp.end());
    cout << cp.size() - 1 << endl;
    for (auto i: cp) {
        cout << i << ' ';
    }


    return 0;
}
