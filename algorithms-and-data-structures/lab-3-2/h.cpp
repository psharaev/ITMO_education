#include <set>
#include <vector>
#include <iostream>
#include <algorithm>
#include <queue>

using namespace std;

typedef vector<int> vi;
typedef vector<vi> vvi;

vector<int> topSort(const vvi &g, vector<int> &cnt_in) {
    vector<int> res;
    queue<int> q;
    int n = (int) g.size();

    for (int i = 0; i < n; ++i) {
        if (cnt_in[i] == 0) {
            q.push(i);
        }
    }

    while (!q.empty()) {
        int v = q.front();
        q.pop();
        for (const int u: g[v]) {
            cnt_in[u]--;
            if (cnt_in[u] == 0) {
                q.push(u);
            }
        }
        res.push_back(v);
    }

    reverse(res.begin(), res.end());

    return res;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n, m, s;
    cin >> n >> m >> s;

    vvi g(n);
    vector<int> cnt_in(n, 0);
    vector<bool> win(n, false);

    for (int i = 0; i < m; i++) {
        int u, v;
        cin >> u >> v;
        g[u - 1].push_back(v - 1);
        cnt_in[v - 1]++;
    }

    auto ts = move(topSort(g, cnt_in));

    for (int u: ts) {
        for (int v: g[u]) {
            win[u] = win[u] || !win[v];
            if (win[u]) {
                break;
            }
        }
    }

    cout << (win[--s] ? "First" : "Second") << " player wins";

    return 0;
}
