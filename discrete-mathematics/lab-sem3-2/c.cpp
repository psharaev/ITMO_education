#include <iostream>
#include <vector>
#include <algorithm>
#include <fstream>

using namespace std;

struct vertex_t {
    int id, w;
};

inline bool operator<(const vertex_t &a, const vertex_t &b) {
    return a.w < b.w;
}

vector<vector<int>> g;
vector<int> LtoR, RtoL;
vector<bool> used;

bool try_kuhn(const int v) {
    if (used[v]) {
        return false;
    }

    used[v] = true;
    for (const auto &to: g[v]) {
        if (LtoR[to] == -1 || try_kuhn(LtoR[to])) {
            LtoR[to] = v;
            RtoL[v] = to;
            return true;
        }
    }

    return false;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    freopen("matching.in", "r", stdin);
    freopen("matching.out", "w", stdout);

    int n;
    cin >> n;

    g.resize(n);
    vector<vertex_t> vertexes(n);

    for (int i = 0; i < n; ++i) {
        vertexes[i].id = i;
        cin >> vertexes[i].w;
    }

    for (int i = 0; i < n; ++i) {
        int k;
        cin >> k;
        for (int j = 0; j < k; ++j) {
            int to;
            cin >> to;
            g[i].push_back(--to);
        }
    }

    sort(vertexes.rbegin(), vertexes.rend());

    LtoR.assign(n, -1);
    RtoL.assign(n, -1);
    for (const vertex_t &v: vertexes) {
        used.assign(n, false);
        try_kuhn(v.id);
    }

    for (const auto &item: RtoL) {
        cout << item + 1 << ' ';
    }

    return 0;
}