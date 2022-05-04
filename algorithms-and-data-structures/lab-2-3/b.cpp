#include <iostream>
#include <vector>

using namespace std;

const int MAX = 200001;
const int LN_MAX = 20;

vector<int> g[MAX];
int timerIn[MAX], timerOut[MAX];
int n, l, timer;
int up[MAX][LN_MAX];

bool check(const int &a, const int &b) {
    return timerIn[a] <= timerIn[b] && timerOut[a] >= timerOut[b];
}

void dfs(const int &v, const int &p) {
    timerIn[v] = timer++;
    up[v][0] = p;

    for (int i = 1; i <= l; ++i) {
        up[v][i] = up[up[v][i - 1]][i - 1];
    }

    for (int i = 0; i < g[v].size(); ++i) {
        int to = g[v][i];
        if (to != p) {
            dfs(to, v);
        }
    }

    timerOut[v] = timer++;
}

int lca(int a, const int &b) {
    if (check(a, b)) {
        return a;
    }

    if (check(b, a)) {
        return b;
    }

    for (int i = l; i >= 0; --i) {
        if (!check(up[a][i], b)) {
            a = up[a][i];
        }
    }

    return up[a][0];
}


int main() {
    scanf("%d", &n);

    for (l = 1; (1 << l) <= n; ++l);

    for (int i = 2; i < n + 1; ++i) {
        int v;
        scanf("%d", &v);
        g[v].push_back(i);
    }
    dfs(1, 1);

    int m;
    scanf("%d", &m);
    for (int i = 0; i < m; ++i) {
        int u, v;
        scanf("%d%d", &u, &v);

        printf("%d\n", lca(u, v));
    }

    return 0;
}