#include <stdio.h>

using namespace std;

const int MAX = 200500;
const int LOGMAX = 18;

int p[MAX];
int depth[MAX];
int dp[MAX][LOGMAX];

int lca(int a, int b) {
    if (depth[a] < depth[b]) {
        return lca(b, a);
    }

    int dif = depth[a] - depth[b];
    int lift = 0;
    while (dif > 0) {
        if (dif & 1) {
            a = dp[a][lift];
        }
        dif >>= 1;
        ++lift;
    }

    for (int i = LOGMAX - 1; i >= 0; --i) {
        if (dp[a][i] != dp[b][i]) {
            a = dp[a][i];
            b = dp[b][i];
        }
    }

    return a != b ? dp[a][0] : a;
}

int get(const int &v) {
    return v == p[v] ? v : (p[v] = get(p[v]));
}

void check(int x, int y) {
    x = get(x);
    y = get(y);
    if (x == y) {
        return;
    }

    depth[x] < depth[y] ? (p[y] = x) : (p[x] = y);
}

int main() {
    int m;
    scanf("%d", &m);
    p[0] = 0;
    int n = 1;
    for (int i = 0; i < m; ++i) {
        char c;
        scanf("%c%c", &c, &c);
        if (c == '+') {
            int v;
            scanf("%d", &v);
            dp[n][0] = v - 1;
            p[n] = n;
            depth[n] = depth[v - 1] + 1;
            for (int j = 1; j < 18; ++j) {
                dp[n][j] = dp[dp[n][j - 1]][j - 1];
            }
            ++n;
        } else if (c == '-') {
            int v;
            scanf("%d", &v);
            check(v - 1, dp[v - 1][0]);
        } else if (c == '?') {
            int u, v;
            scanf("%d%d", &u, &v);
            printf("%d\n", get(lca(u - 1, v - 1)) + 1);
        }
    }
}