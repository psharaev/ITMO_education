#include <iostream>
#include <vector>
#include <algorithm>
#include <fstream>

using namespace std;

typedef long long ll;

struct edge_t {
    int u, v, id;
    ll w;
    bool isUsed{false};
};

ll n, m, s;

vector<edge_t> edges;
vector<int> dsu_p, sz;

std::ostream &operator<<(std::ostream &os, const edge_t &e) {
    return os << "{ id: " << e.id << " u: " << e.u << " v: " << e.v << " w: " << e.w << " isUsed: " << e.isUsed << " }";
}

template<typename T>
void dump(const vector<T> &v) {
    for (const T &item: v) {
        cout << item << ' ';
    }
    cout << '\n';
}

inline bool operator<(const edge_t& a, const edge_t& b) {
    return a.w < b.w;
}

int dsu_get(int v) {
    if (v == dsu_p[v]) {
        return v;
    }
    return dsu_p[v] = dsu_get(dsu_p[v]);
}

void dsu_unite(int x, int y) {
    x = dsu_get(x);
    y = dsu_get(y);

    if (x == y) {
        return;
    }

    if (sz[x] < sz[y]) {
        swap(x, y);
    }
    dsu_p[y] = x;
    sz[x] += sz[y];
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    freopen("destroy.in", "r", stdin);
    freopen("destroy.out", "w", stdout);

    cin >> n >> m >> s;
    edges.resize(m);
    dsu_p.resize(n);
    for (int i = 0; i < n; ++i) {
        dsu_p[i] = i;
    }
    sz.assign(n, 1);

    for (int i = 0; i < m; ++i) {
        cin >> edges[i].u >> edges[i].v >> edges[i].w;
        edges[i].u--;
        edges[i].v--;
        edges[i].id = i + 1;
    }

    sort(edges.rbegin(), edges.rend());
    for (auto &e: edges) {
        if (dsu_get(e.u) != dsu_get(e.v)) {
            dsu_unite(e.u, e.v);
        } else {
            e.isUsed = true;
        }
    }

    vector<int> res;
    for (auto e = edges.rbegin(); e != edges.rend(); e++) {
        if (e->isUsed) {
            if ((s -= e->w) >= 0) {
                res.push_back(e->id);
            } else {
                break;
            }
        }
    }

    sort(res.begin(), res.end());
    cout << res.size() << '\n';
    for (auto &r: res) {
        cout << r << ' ';
    }

    return 0;
}