#include <iostream>
#include <vector>
#include <algorithm>
#include <set>

using namespace std;

typedef unsigned long long ll;

typedef vector<ll> vi;
typedef vector<pair<ll, ll>> graph_unit;
typedef vector<graph_unit> graph;
typedef vector<bool> vb;
typedef vector<vb> vvb;

set<ll> ans;

void bridgeUtil(const graph &g, const ll &u, vb &visited, vi &disc, vi &low, vi &parent) {
    static ll time = 0;
    visited[u] = true;
    disc[u] = low[u] = ++time;

    for (const auto &v: g[u]) {
        if (!visited[v.second]) {
            parent[v.second] = u;
            bridgeUtil(g, v.second, visited, disc, low, parent);

            low[u] = min(low[u], low[v.second]);

            if (low[v.second] > disc[u]) {
                ans.insert(v.first);
            }
        } else if (v.second != parent[u]) {
            low[u] = min(low[u], disc[v.second]);
        }
    }
}

void bridge(const graph &g, const ll &n) {
    vb visited(n, false);
    vi disc(n);
    vi low(n);
    vi parent(n, -1);

    for (ll i = 0; i < n; i++) {
        if (!visited[i]) {
            bridgeUtil(g, i, visited, disc, low, parent);
        }
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    ll n, m;
    cin >> n >> m;

    graph g(n, graph_unit());

    for (ll i = 1; i <= m; ++i) {
        ll u, v;
        cin >> u >> v;
        g[u - 1].emplace_back(i, v - 1);
        g[v - 1].emplace_back(i, u - 1);
    }

    bridge(g, n);

    cout << ans.size() << endl;
    for (const ll &i: ans) {
        cout << i << endl;
    }

    return 0;
}
