#include <iostream>
#include <vector>
#include <set>

using namespace std;

typedef long long ll;
typedef vector<ll> vi;
typedef vector<vi> vvi;

#define INF LONG_MAX

#define SIZE 901

struct edge_t {
    ll u, v, c, w, f = 0;

    explicit edge_t(const ll &u, const ll &v, const ll &c, const ll &w) : u(u), v(v), c(c), w(w) {}
};

typedef vector<edge_t> ve;

vi p, from, d;
ve edges;
vvi g;

void bfs() {
    d.assign(SIZE, INF);
    d[0] = 0;

    set<pair<ll, ll>> q;
    from.assign(SIZE, -1);
    q.insert({0, 0});
    while (!q.empty()) {
        const ll u = q.begin()->second;
        q.erase(q.begin());
        for (const ll &id: g[u]) {
            const edge_t &e = edges[id];
            const ll delta = e.w + p[u] - p[e.v];
            if (e.f < e.c && d[u] + delta < d[e.v]) {
                q.erase({d[e.v], e.v});
                d[e.v] = d[u] + delta;
                from[e.v] = id;
                q.insert({d[e.v], e.v});
            }
        }
    }
}

ll dfs(const ll &t, const ll &flow, ll &ans) {
    const ll num = from[t];
    if (num == -1) {
        return flow;
    }
    edge_t &uv = edges[num];
    ans += uv.w;
    ll delta = dfs(uv.u, min(flow, uv.c - uv.f), ans);
    uv.f += delta;
    edges[num ^ 1].f -= delta;
    return delta;
}


int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    ll n;
    cin >> n;

    g.assign(SIZE, vi());

    ll edges_size = 0;
    for (ll i = 1; i <= n; i++) {
        for (ll j = 1; j <= n; j++) {
            ll w;
            cin >> w;
            g[i].push_back(edges_size++);
            edges.emplace_back(i, n + j, 1, w);
            g[n + j].push_back(edges_size++);
            edges.emplace_back(n + j, i, 0, -w);
        }
    }

    for (ll i = 1; i <= n; i++) {
        g[0].push_back(edges_size++);
        edges.emplace_back(0, i, 1, 0);
        g[i].push_back(edges_size++);
        edges.emplace_back(i, 0, 0, 0);
    }

    const ll t = 2 * n + 1;
    for (ll i = n + 1; i <= 2 * n; i++) {
        g[i].push_back(edges_size++);
        edges.emplace_back(i, t, 1, 0);
        g[t].push_back(edges_size++);
        edges.emplace_back(t, i, 0, 0);
    }

    p.assign(SIZE, 0);
    bfs();
    p = d;
    ll ans = 0;

    while (d[t] != INF) {
        dfs(t, 1, ans);
        bfs();
        for (ll i = 0; i < SIZE; ++i) {
            p[i] += d[i];
        }
    }

    cout << ans << '\n';
    for (const edge_t &now: edges) {
        if (now.v != t && now.u != 0 && now.f == 1) {
            cout << now.u << ' ' << now.v - n << '\n';
        }
    }
}
