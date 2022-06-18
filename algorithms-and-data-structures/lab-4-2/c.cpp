#include <iostream>
#include <vector>
#include <queue>

using namespace std;

typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef queue<int> qi;

int n, m, s, t, n_1;

struct edge_t {
    int u, v, c;
    int f = 0;

    edge_t(const int &u, const int &v, const int &c) : u(u), v(v), c(c) {}
};

vector<edge_t> edges;
vi d;

bool bfs(const vvi &g) {
    d.assign(n_1, n_1);
    d[s] = 0;
    queue<int> q;
    q.push(s);
    while (!q.empty()) {
        const int v = q.front();
        q.pop();
        for (const int &index: g[v]) {
            const edge_t &uv = edges[index];
            if (uv.f < uv.c && d[uv.v] == n_1) {
                d[uv.v] = d[v] + 1;
                q.push(uv.v);
            }
        }
    }
    return d[t] != n_1;
}

int dfs(const vvi &g, vi &p, const int &v, const int &minCut) {
    if (v == t || minCut == 0) {
        return minCut;
    }
    while (p[v] < g[v].size()) {
        edge_t &uv = edges[g[v][p[v]]];
        if (d[uv.v] == d[v] + 1) {
            const int delta = dfs(g, p, uv.v, min(minCut, uv.c - uv.f));
            if (delta != 0) {
                uv.f += delta;
                edges[g[v][p[v]] ^ 1].f -= delta;
                return delta;
            }
        }
        p[v]++;
    }
    return 0;
}

void dfs_getAns(const vvi &g, const int &v, vector<int> &ans) {
    if (v == t) {
        ans.push_back(v);
        return;
    }
    for (const int &u: g[v]) {
        if (edges[u].f == 1) {
            edges[u].f = 0;
            ans.push_back(v);
            dfs_getAns(g, edges[u].v, ans);
            return;
        }
    }
}

void dump(const vi &a) {
    for (const int &item: a) {
        cout << item << ' ';
    }
    cout << '\n';
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    cin >> n >> m >> s >> t;

    n_1 = n + 1;
    vvi g(n_1);

    for (int i = 0; i < m; i++) {
        int u, v;
        cin >> u >> v;
        g[u].push_back(2 * i);
        edges.emplace_back(u, v, 1);
        g[v].push_back(2 * i + 1);
        edges.emplace_back(v, u, 0);
    }

    int maxFlow = 0;
    while (bfs(g)) {
        vi p(n_1, 0);
        while (true) {
            int flow = dfs(g, p, s, n_1);
            if (flow == 0) {
                break;
            }
            maxFlow += flow;
        }
    }

    if (maxFlow < 2) {
        cout << "NO\n";
        return 0;
    }

    cout << "YES\n";

    vi ans;
    dfs_getAns(g, s, ans);
    dump(ans);
    ans.clear();
    dfs_getAns(g, s, ans);
    dump(ans);
}