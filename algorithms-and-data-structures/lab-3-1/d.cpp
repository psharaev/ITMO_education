#include <iostream>
#include <algorithm>
#include "map"
#include "vector"
#include "set"

using namespace std;

typedef long long ll;

typedef vector<ll> vi;
typedef vector<bool> vb;
typedef map<ll, set<ll>> graph_t;

graph_t g;
vb used;
vi t_out, t_in, res;
map<pair<ll, ll>, ll> gg;
vector<pair<ll, ll>> b;
ll cnt = 0;

void dfs(const ll &v, const ll &p) {
    static ll t = 0;
    t_out[v] = t_in[v] = ++t;
    used[v] = true;
    for (auto u: g[v]) {
        if (u == p) {
            continue;
        }

        if (!used[u]) {
            dfs(u, v);
            if (t_out[v] > t_out[u]) {
                t_out[v] = t_out[u];
            }
        } else {
            if (t_out[v] > t_in[u]) {
                t_out[v] = t_in[u];
            }
        }
    }

    if (p != -1 && t_out[v] == t_in[v] && gg[{max(v, p), min(v, p)}] == 1) {
        b.emplace_back(p, v);
    }
}

void dfs_check(const ll &v) {
    used[v] = false;
    for (auto i: g[v]) {
        if (used[i]) {
            dfs_check(i);
        }
    }
    res[v - 1] = cnt;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    ll n, m;
    cin >> n >> m;

    used.resize(n + 1, false);
    res.resize(n);
    t_in.resize(n + 1);
    t_out.resize(n + 1);

    for (ll i = 0; i < m; ++i) {
        ll u, v;
        cin >> u >> v;
        gg[{max(u, v), min(u, v)}]++;
        g[u].insert(v);
        g[v].insert(u);
    }

    for (ll v = 1; v <= n; ++v) {
        if (!used[v]) {
            dfs(v, -1);
        }
    }

    for (const auto& e: b) {
        ll p = e.first, v = e.second;
        g[p].erase(v);
        g[v].erase(p);
    }

    for (ll v = 1; v <= n; ++v) {
        if (used[v]) {
            cnt++;
            dfs_check(v);
        }
    }

    cout << cnt << endl;
    for (const auto& v: res) {
        cout << v << ' ';
    }

    return 0;
}