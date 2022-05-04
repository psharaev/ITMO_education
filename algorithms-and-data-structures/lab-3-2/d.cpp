#include <iostream>
#include <vector>

using namespace std;

typedef long long ll;

#define INF LONG_LONG_MAX

struct edge_t {
    ll u, v, w;

    edge_t() = default;

    edge_t(const ll &u, const ll &v, const ll &w) : u(u), v(v), w(w) {}
};


int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    ll n, m, k, s;
    cin >> n >> m >> k >> s;
    s--;

    vector<edge_t> g(m);

    for (ll i = 0; i < m; ++i) {
        ll u, v, w;
        cin >> u >> v >> w;
        g[i] = {u - 1, v - 1, w};
    }

    vector<vector<ll>> d(k + 1, vector<ll>(n, INF));
    d[0][s] = 0;

    for (ll i = 0; i < k; ++i) {
        for (const edge_t &e: g) {
            if (d[i][e.u] < INF) {
                d[i + 1][e.v] = min(d[i + 1][e.v], d[i][e.u] + e.w);
            }
        }
    }

    for (ll i = 0; i < n; ++i) {
        if (d[k][i] == INF) {
            cout << -1 << '\n';
        } else {
            cout << d[k][i] << '\n';
        }
    }

    return 0;
}