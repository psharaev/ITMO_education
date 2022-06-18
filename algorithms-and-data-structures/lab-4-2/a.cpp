#include <iostream>
#include <vector>

using namespace std;

typedef long long ll;
typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;

int n, m;

struct edge_t {
    int u, v, c;
    ll f = 0;

    edge_t(const int &u, const int &v, const int &c) : u(u), v(v), c(c) {}
};

vector<edge_t> edges;

ll dfs(const vvi &g, vb &used, const int &u, const ll &flow) {
    if (u == n) {
        return flow;
    }
    if (used[u]) {
        return 0;
    }

    used[u] = true;
    for (int v: g[u]) {
        edge_t &uv = edges[v];
        if (uv.f < uv.c) {
            ll delta = dfs(g, used, uv.v, min(flow, uv.c - uv.f));
            if (delta > 0) {
                uv.f += delta;
                edges[v ^ 1].f -= delta;
                return delta;
            }
        }
    }

    return 0;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    cin >> n >> m;
    vvi g(n + 1);
    for (int i = 0; i < m; i++) {
        int u, v, c;
        cin >> u >> v >> c;
        g[u].push_back(2 * i);
        edges.emplace_back(u, v, c);
        g[v].push_back(2 * i + 1);
        edges.emplace_back(v, u, c);
    }

    ll ans = 0;
    while (true) {
        vb used(n + 1, false);
        ll delta = dfs(g, used, 1, LONG_MAX);
        if (delta == 0) {
            break;
        }
        ans += delta;
    }

    cout << ans << '\n';
    for (int i = 0; i < m; i++) {
        cout << edges[2 * i].f << '\n';
    }

    return 0;
}