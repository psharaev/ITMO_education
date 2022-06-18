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

typedef vsi graph_t;

int n, m, k;

void dfs(const graph_t &g, const int &v, vi &c) {
    if (c[v] != -1) {
        return;
    }

    vb used_color(k, false);
    for (const auto &u: g[v]) {
        if (c[u] != -1) {
            used_color[c[u]] = true;
        }
    }

    for (int i = 0; i < k; ++i) {
        if (!used_color[i]) {
            c[v] = i;
            break;
        }
    }

    for (const auto &u: g[v]) {
        dfs(g, u, c);
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    cin >> n >> m;

    graph_t g(n, si());
    vi c(n, -1);

    for (int i = 0; i < m; ++i) {
        int a, b;
        cin >> a >> b;
        a--;
        b--;
        g[b].insert(a);
        g[a].insert(b);
    }

    vi deg(n, 0);

    int maxDegId = 0;
    int maxDegValue = (int) g[0].size();
    for (int i = 1; i < n; ++i) {
        deg[i] = (int) g[i].size();
        if (maxDegValue < deg[i]) {
            maxDegId = i;
            maxDegValue = deg[i];
        }
    }

    k = maxDegValue;
    if (k % 2 == 0) {
        k++;
    }
    if (k == 1) {
        k = 3;
    }

    dfs(g, maxDegId, c);

    cout << k << endl;
    for (const auto &i: c) {
        cout << i + 1<< endl;
    }

    return 0;
}
