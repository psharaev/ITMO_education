#include <stdio.h>
#include <vector>

using namespace std;

const int MAX = 300000;
const int LOGMAX = 19;

vector<vector<int>> ways;
int dp[MAX][LOGMAX], up[MAX];
int timerIn[MAX], timerOut[MAX];
long long tree[4 * MAX];
int border = 0;

void update(const int &id, const int &l, const int &r, const int &pos, const long long &v) {
    if (l == r) {
        tree[id] += v;
        return;
    }

    if (l > r) {
        return;
    }

    int m = (l + r) / 2;
    if (pos <= m) {
        update(2 * id + 1, l, m, pos, v);
    } else {
        update(2 * id + 2, m + 1, r, pos, v);
    }
    tree[id] = tree[2 * id + 1] + tree[2 * id + 2];
}

long long sum(const int &id, const int &left, const int &right, const int &l, const int &r) {
    if (l > right || r < left || l > r || left > right) {
        return 0;
    }

    if (left == l && right == r) {
        return tree[id];
    }

    int m = (left + right) / 2;
    return sum(2 * id + 1, left, m, l, min(m, r)) +
           sum(2 * id + 2, m + 1, right, max(m + 1, l), r);
}

bool check(const int &a, const int &b) {
    return timerIn[a] <= timerIn[b] && timerOut[b] <= timerOut[a];
}

void dfs(const int &v, const int &p) {
    timerIn[v] = border++;
    up[v] = p == -1 ? 0 : p;

    for (const auto &w : ways[v]) {
        if (w != p) {
            dfs(w, v);
        }
    }

    timerOut[v] = border - 1;
}

int lca(int a, const int &b) {
    if (check(a, b)) {
        return a;
    }

    for (int i = LOGMAX - 1; i >= 0; --i) {
        if (!check(dp[a][i], b)) {
            a = dp[a][i];
        }
    }
    return up[a];
}

int main() {
    int n;
    scanf("%d", &n);
    ways.resize(n + 1);
    ways[0].push_back(1);
    ways[1].push_back(0);
    for (int i = 0; i < n - 1; ++i) {
        int v, u;
        scanf("%d%d", &v, &u);
        ways[v].push_back(u);
        ways[u].push_back(v);
    }

    dfs(0, -1);


    for (int i = 0; i <= n; ++i) {
        dp[i][0] = up[i] < 0 ? i : up[i];
    }

    for (int j = 1; j < LOGMAX; ++j) {
        for (int i = 0; i <= n; ++i) {
            dp[i][j] = dp[dp[i][j - 1]][j - 1];
        }
    }

    int m;
    scanf("%d", &m);
    for (int i = 0; i < m; ++i) {
        char c;
        scanf("%c%c", &c, &c);
        if (c == '+') {
            int v, u;
            long long d;
            scanf("%d%d%lld", &v, &u, &d);
            update(0, 0, n + 2, timerIn[v], d);
            update(0, 0, n + 2, timerIn[u], d);
            update(0, 0, n + 2, timerIn[lca(v, u)], -d);
            update(0, 0, n + 2, timerIn[up[lca(v, u)]], -d);
        } else if (c == '?') {
            int v;
            scanf("%d", &v);
            printf("%lld\n", sum(0, 0, n + 2, timerIn[v], timerOut[v]));
        }
    }
}