#include <set>
#include <vector>
#include <iostream>
#include <algorithm>

using namespace std;

typedef long long ll;

#define INF LONG_LONG_MAX

struct edge_to_t {
    int v;
    ll w;

    edge_to_t() = default;

    edge_to_t(const int &v, const ll &w) : v(v), w(w) {}
};

inline bool operator<(const edge_to_t &a, const edge_to_t &b) {
    return a.w < b.w || (b.w >= a.w && a.v < b.v);
}

vector<bool> used;
vector<vector<edge_to_t>> g;

void dfs(const int &v) {
    used[v] = true;
    for (const auto &e: g[v]) {
        if (!used[e.v]) {
            dfs(e.v);
        }
    }
}


void dijkstra(const int &start, const int &n, vector<ll> &d) {
    d[start] = 0;

    set<edge_to_t> q;
    q.insert({start, 0});

    while (!q.empty()) {
        auto u = *q.begin();
        q.erase(q.begin());

        for (auto e: g[u.v]) {
            if (d[e.v] > d[u.v] + e.w) {
                q.erase({e.v, d[e.v]});
                d[e.v] = d[u.v] + e.w;
                q.insert({e.v, d[e.v]});
            }
        }
    }
}

template<typename T>
inline T min(const T &a, const T &b, const T &c) {
    return min(a, min(b, c));
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n, m, a, b, c;

    cin >> n >> m;

    used.resize(n, false);
    g.resize(n, vector<edge_to_t>(0));

    for (int i = 0; i < m; i++) {
        int u, v, w;
        cin >> u >> v >> w;
        u--;
        v--;
        g[u].emplace_back(v, w);
        g[v].emplace_back(u, w);
    }

    cin >> a >> b >> c;
    a--;
    b--;
    c--;

    dfs(a);
    if (!used[b] || !used[c]) {
        cout << -1 << endl;
        return 0;
    }

    used.assign(n, false);

    dfs(b);
    if (!used[a] || !used[c]) {
        cout << -1 << endl;
        return 0;
    }

    used.assign(n, false);

    dfs(c);
    if (!used[a] || !used[b]) {
        cout << -1 << endl;
        return 0;
    }

    vector<ll> dA(n, INF), dB(n, INF);
    dijkstra(a, n, dA);
    dijkstra(b, n, dB);

    ll a_b = dA[b];
    ll a_c = dA[c];
    ll b_c = dB[c];

    ll res = min(a_b + b_c, // abc
                 a_b + a_c, // cab
                 b_c + a_c); // bca

    cout << res << endl;

    return 0;
}
