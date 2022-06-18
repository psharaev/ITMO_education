#include <cmath>
#include <vector>
#include <stdio.h>
#include <algorithm>

using namespace std;

const int LOG_MAX = 19;
const int MAX = 100001;

int timerIn[MAX], firstTimerIn[MAX], depth[MAX];

int root, time, pos;

vector<int> tree[MAX];
vector<vector<pair<int, int>>> table(LOG_MAX, vector<pair<int, int>>(2 * MAX, {0, 0}));

bool comp(const int &first, const int &second) {
    return timerIn[first] < timerIn[second];
}

void dfs(const int &v, const int &p) {
    
    timerIn[v] = time++;
    firstTimerIn[v] = pos;
    table[0][pos++] = {depth[v] = p, v};

    for (const int &nextV : tree[v]) {
        dfs(nextV, p + 1);
        table[0][pos] = {p, v};
        pos += 1;
    }
}

int lca(const int &a, const int &b) {
    int min_ = min(firstTimerIn[a], firstTimerIn[b]);
    int max_ = max(firstTimerIn[a], firstTimerIn[b]);
    int len_ = max_ - min_ + 1;
    int pow_ = (int) log2(len_);
    return min(table[pow_][min_], table[pow_][max_ - (1 << pow_) + 1]).second;
}

int main() {

    int n;
    scanf("%d", &n);

    for (int i = 0; i < n; i++) {
        int t;
        scanf("%d", &t);
        if (t == -1) {
            root = i;
        } else {
            t -= 1;
            tree[t].push_back(i);
        }
    }

    dfs(root, 1);

    for (int i = 1; i < LOG_MAX; ++i) {
        int pow_ = 1 << (i - 1);
        for (int j = 0; j + pow_ < pos; ++j) {
            table[i][j] = min(table[i - 1][j], table[i - 1][j + pow_]);
        }
    }

    int m;
    scanf("%d", &m);

    int q[MAX];

    for (int i = 0; i < m; ++i) {
        int k;
        scanf("%d", &k);

        if (k == 0) {
            printf("0\n");
            continue;
        }

        for (int j = 0; j < k; ++j) {
            scanf("%d", &q[j]);
            q[j]--;
        }

        sort(q, q + k, &comp);

        int result = depth[q[0]];
        for (int j = 1; j < k; ++j) {
            result += depth[q[j]] - depth[lca(q[j - 1], q[j])];
        }

        printf("%d\n", result);
    }

    return 0;
}