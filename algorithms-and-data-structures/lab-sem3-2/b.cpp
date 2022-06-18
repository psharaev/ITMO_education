#include <iostream>
#include <vector>
#include <set>

using namespace std;

typedef long long ll;

#define INF LONG_LONG_MAX

struct edge_to_t {
    ll v, w;

    edge_to_t() = default;

    edge_to_t(const ll &v, const ll &w) : v(v), w(w) {}
};

inline bool operator<(const edge_to_t &a, const edge_to_t &b) {
    return a.w < b.w || (b.w >= a.w && a.v < b.v);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    ll n, m;
    cin >> n >> m;

    vector<vector<edge_to_t>> g(n);

    for (ll i = 0; i < m; ++i) {
        ll u, v, w;
        cin >> u >> v >> w;
        g[u - 1].emplace_back(v - 1, w);
        g[v - 1].emplace_back(u - 1, w);
    }

    vector<ll> d(n, INF);
    d[0] = 0;
    set<edge_to_t> q;
    q.emplace(0, 0);
    while (!q.empty()) {
        ll u = q.begin()->v;
        q.erase(q.begin());

        for (auto uv: g[u]) {
            ll v = uv.v;
            ll w = uv.w;
            if (d[v] > d[u] + w) {
                d[v] = d[u] + w;
                q.erase(uv);
                q.emplace(v, d[v]);
            }
        }
    }

    for (const ll &i: d) {
        cout << i << ' ';
    }

    return 0;
}