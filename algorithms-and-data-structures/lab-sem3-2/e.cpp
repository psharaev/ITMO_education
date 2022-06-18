#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

typedef long long ll;

#define INF (ll)1e19

struct edge_t {
    ll u, v, w;

    edge_t() = default;

    edge_t(const ll &u, const ll &v, const ll &w) : u(u), v(v), w(w) {}
};

vector<edge_t> edges;
vector<vector<ll>> g;
vector<bool> used;
vector<ll> d;

void dfs(const ll &v) {
    used[v] = true;
    for (const ll &to: g[v]) {
        if (!used[to]) {
            dfs(to);
        }
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    ll n, m, s;

    cin >> n >> m >> s;
    s--;

    d.assign(n, INF);
    g.resize(n);
    used.resize(n, false);

    for (ll i = 0; i < m; ++i) {
        ll u, v;
        ll w;
        cin >> u >> v >> w;
        u--;
        v--;
        g[u].push_back(v);
        edges.emplace_back(u, v, w);
    }

    d[s] = 0;
    for (ll i = 0; i < n; ++i) {
        for (const edge_t &e: edges) {
            if (d[e.u] < INF && (d[e.v] > d[e.u] + e.w)) {
                d[e.v] = max(-INF, d[e.u] + e.w);
            }
        }
    }

    for (ll i = 0; i < n; ++i) {
        for (const edge_t &e: edges) {
            if (d[e.u] < INF && (d[e.v] > d[e.u] + e.w) && !used[e.v]) {
                dfs(e.v);
            }
        }
    }


    for (ll i = 0; i < n; ++i) {
        if (d[i] == INF) {
            cout << "*\n";
        } else if (used[i]) {
            cout << "-\n";
        } else {
            cout << d[i] << '\n';
        }
    }

    return 0;
}