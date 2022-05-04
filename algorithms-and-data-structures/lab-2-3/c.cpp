#include <iostream>
#include <vector>

using namespace std;

const int MAX = 200001;
const int LOGMAX = 20;

vector<pair<int, int>> g[MAX];
int timerIn[MAX], timerOut[MAX];
int n, l, timer;
pair<int, int> up[MAX][LOGMAX];

bool check(const int &a, const int &b) {
    return timerIn[a] <= timerIn[b] && timerOut[a] >= timerOut[b];
}

void dfs(const int &v, const int &p, const int &c = INT_MAX) {
    timerIn[v] = timer++;
    up[v][0] = make_pair(p, c);

    for (int i = 1; i <= l; ++i) {
        up[v][i].first = up[up[v][i - 1].first][i - 1].first;
        up[v][i].second = min(up[v][i - 1].second, up[up[v][i - 1].first][i - 1].second);
    }

    for (int i = 0; i < g[v].size(); ++i) {
        dfs(g[v][i].first, v, g[v][i].second);
    }

    timerOut[v] = timer++;
}

int minlca(int a, int b) {
    int res = INT_MAX;
    for (int i = l; i >= 0; --i) {
        if (!check(up[a][i].first, b)) {
            res = min(res, up[a][i].second);
            a = up[a][i].first;
        }
    }

    if (!check(a, b)) {
        res = min(res, up[a][0].second);
    }

    for (int i = l; i >= 0; --i) {
        if (!check(up[b][i].first, a)) {
            res = min(res, up[b][i].second);
            b = up[b][i].first;
        }
    }

    if (!check(b, a)) {
        res = min(res, up[b][0].second);
    }

    return res;
}


int main() {
    scanf("%d", &n);

    for (l = 1; (1 << l) <= n; ++l);

    for (int i = 2; i < n + 1; ++i) {
        int x, p;
        scanf("%d%d", &x, &p);
        g[x].emplace_back(i, p);
    }
    dfs(1, 1);

    int m;
    scanf("%d", &m);
    for (int i = 0; i < m; ++i) {
        int u, v;
        scanf("%d%d", &u, &v);

        printf("%d\n", minlca(u, v));
    };

    return 0;
}