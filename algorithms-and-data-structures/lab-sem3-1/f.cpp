#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <deque>

using namespace std;

typedef long long ll;
typedef map<ll, set<ll>> graph_t;

graph_t t_in, t_out;
vector<ll> used;
deque<ll> s;
ll cnt = 1;

void dfs_i(const ll &v) {
    used[v] = 0;
    for (const ll &i: t_in[v]) {
        if (used[i] == -1) {
            dfs_i(i);
        }
    }
    s.push_front(v);
}

void dfs_o(const ll &v) {
    used[v] = cnt;
    for (const ll &i: t_out[v]) {
        if (used[i] == 0) {
            dfs_o(i);
        }
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    ll n, m;
    cin >> n >> m;

    used.resize(n + 1, -1);

    for (ll i = 0; i < m; ++i) {
        ll u, v;
        cin >> u >> v;
        t_in[u].insert(v);
        t_out[v].insert(u);
    }

    for (ll i = 1; i <= n; ++i) {
        if (used[i] == -1) {
            dfs_i(i);
        }
    }

    for (const ll &v: s) {
        if (used[v] == 0) {
            cnt++;
            dfs_o(v);
        }
    }

    set<pair<ll, ll>> ans;
    for (ll u = 1; u <= n; ++u) {
        for (const ll &v: t_out[u]) {
            if (used[u] != used[v]) {
                ans.insert({used[u], used[v]});
            }
        }
    }

    cout << ans.size() << endl;

    return 0;
}
