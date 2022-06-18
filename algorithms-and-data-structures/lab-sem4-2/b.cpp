#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>
#include <map>

using namespace std;

typedef long long ll;
typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef queue<int> qi;


int n, m;
int n_1;

struct edge_t {
    int u, v, c;
    ll f = 0;

    edge_t(const int &u, const int &v, const int &c) : u(u), v(v), c(c) {}
};

bool operator<(const edge_t &e1, const edge_t &e2) {
    return tie(e1.u, e1.v) < tie(e2.u, e2.v);
}

vector<edge_t> edges;
vi d;

bool bfs_1(const vvi &g) {
    d.assign(n_1, n_1);
    d[1] = 0;
    qi q;
    q.push(1);

    while (!q.empty()) {
        int u = q.front();
        q.pop();
        for (const int &id: g[u]) {
            edge_t uv = edges[id];
            if (uv.f < uv.c && d[uv.v] == n_1) {
                d[uv.v] = d[u] + 1;
                q.push(uv.v);
            }
        }
    }

    return d[n] != n_1;
}

void bfs_2(const vvi &g, vb &used) {
    used[1] = true;
    qi q;
    q.push(1);

    while (!q.empty()) {
        int u = q.front();
        q.pop();
        for (const int &id: g[u]) {
            edge_t uv = edges[id];
            if (uv.f < uv.c && !used[uv.v]) {
                used[uv.v] = true;
                q.push(uv.v);
            }
        }
    }
}

ll dfs(const vvi &g, vi &p, const int &u, const ll &minC) {
    if (u == n || minC == 0) {
        return minC;
    }

    while (p[u] < g[u].size()) {
        edge_t &uv = edges[g[u][p[u]]];
        if (d[uv.v] == d[u] + 1) {
            ll delta = dfs(g, p, uv.v, min(minC, uv.c - uv.f));
            if (delta != 0) {
                uv.f += delta;
                edges[g[u][p[u]] ^ 1].f -= delta;
                return delta;
            }
        }
        p[u]++;
    }

    return 0;
}


int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);
    cin >> n >> m;
    n_1 = n + 1;
    vvi g(n_1);
    map<edge_t, int> edgeToId;
    for (int i = 0; i < m; ++i) {
        int u, v, c;
        cin >> u >> v >> c;
        g[u].push_back(2 * i);
        edges.emplace_back(u, v, c);
        edgeToId[edges.back()] = i;
        g[v].push_back(2 * i + 1);
        edges.emplace_back(v, u, c);
        edgeToId[edges.back()] = i;
    }

    ll maxFlow = 0;
    while (bfs_1(g)) {
        vi p(n_1, 0);
        while (true) {
            ll fl = dfs(g, p, 1, n_1);
            if (fl == 0) {
                break;
            }
            maxFlow += fl;
        }
    }

    vb used_bfs2(n_1, false);
    bfs_2(g, used_bfs2);
    vi ans;
    for (const edge_t &it: edges) {
        if (used_bfs2[it.u] && !used_bfs2[it.v]) {
            ans.push_back(edgeToId[it]);
        }
    }

    cout << ans.size() << ' ' << maxFlow << '\n';
    for_each(ans.begin(), ans.end(), [&](const int &it) { cout << it + 1 << ' '; });
}