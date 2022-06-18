#include <iostream>
#include <vector>

using namespace std;

typedef long long ll;
typedef vector<ll> vi;
typedef vector<vi> vvi;

#define INF 1000000007

struct edge_t {
    int to;
    ll currentF, f, cost;

    explicit edge_t(const int &to,
                    const ll &currentF,
                    const ll &f,
                    const ll &cost) : to(to), currentF(currentF), f(f), cost(cost) {}
};

typedef vector<edge_t> ve;

int n, m;

bool pushFlow(const vvi &g, ve &edges) {
    vi d(g.size(), INF);
    d[0] = 0;
    vector<pair<ll, ll> > parent(g.size(), make_pair(-1, -1));

    for (size_t i = 0; i < g.size(); i++) {
        for (size_t j = 0; j < g.size(); j++) {
            if (d[j] == INF) {
                continue;
            }
            for (const ll &uv: g[j]) {
                if (edges[uv].currentF == edges[uv].f) {
                    continue;
                }
                if (d[edges[uv].to] > d[j] + edges[uv].cost) {
                    d[edges[uv].to] = d[j] + edges[uv].cost;
                    parent[edges[uv].to] = make_pair(j, uv);
                }
            }
        }
    }

    ll goo = INF;
    pair<ll, ll> cur = make_pair(n - 1, -1);
    vi path;
    while (true) {
        if (cur.second != -1) {
            path.push_back(cur.second);
            goo = min(goo, edges[cur.second].f - edges[cur.second].currentF);
        }
        if (parent[cur.first].first == -1) {
            break;
        }
        cur = parent[cur.first];
    }
    for (const ll &i: path) {
        edges[i].currentF += goo;
        edges[i ^ 1].currentF -= goo;
    }

    return !path.empty();
}

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    cin >> n >> m;

    vvi g(n);
    vector<edge_t> edges;

    for (int i = 0; i < m; i++) {
        int from, to;
        ll f, cost;
        cin >> from >> to >> f >> cost;
        from--;
        to--;
        edges.emplace_back(to, 0, f, cost);
        g[from].push_back(i * 2);
        edges.emplace_back(from, 0, 0, -cost);
        g[to].push_back(i * 2 + 1);
    }

    while (pushFlow(g, edges)) {}

    ll ans = 0ll;
    for (size_t i = 0; i < edges.size(); i += 2) {
        ans += edges[i].currentF * edges[i].cost;
    }
    cout << ans << '\n';

    return 0;
}