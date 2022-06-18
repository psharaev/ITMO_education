#include <iostream>
#include <vector>
#include <algorithm>
#include <set>

using namespace std;

typedef unsigned long long ll;

typedef vector<ll> vi;
typedef vector<ll> graph_unit;
typedef vector<graph_unit> graph;
typedef vector<bool> vb;
typedef vector<vb> vvb;

set<ll> ans;

void APUtil(const graph &g, const ll &u, vb &visited, vi &disc, vi &low, const ll &parent, vb &isAP) {
    int children = 0;
    static ll time = 0;
    visited[u] = true;
    disc[u] = low[u] = ++time;

    for (const auto &v: g[u]) {
        if (!visited[v]) {
            children++;
            APUtil(g, v, visited, disc, low, u, isAP);

            low[u] = min(low[u], low[v]);

            if (parent != -1 && low[v] >= disc[u]) {
                isAP[u] = true;
            }
        } else if (v != parent) {
            low[u] = min(low[u], disc[v]);
        }
    }

    if (parent == -1 && children > 1) {
        isAP[u] = true;
    }
}

void AP(const graph &g, const ll &n) {
    vi disc(n, 0);
    vi low(n);
    vb visited(n, false);
    vb isAP(n, false);
    ll par = -1;

    for (ll u = 0; u < n; u++) {
        if (!visited[u]) {
            APUtil(g, u, visited, disc, low, par, isAP);
        }
    }

    for (ll u = 0; u < n; u++) {
        if (isAP[u]) {
            ans.insert(u);
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

    for (ll i = 0; i < m; ++i) {
        ll u, v;
        cin >> u >> v;
        g[u - 1].push_back(v - 1);
        g[v - 1].push_back(u - 1);
    }

    AP(g, n);

    cout << ans.size() << endl;
    for (const ll &i: ans) {
        cout << i + 1 << ' ';
    }

    return 0;
}
