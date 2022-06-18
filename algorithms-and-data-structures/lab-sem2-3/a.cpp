#include <cmath>
#include <vector>
#include <iostream>
#include <algorithm>

using namespace std;

vector<bool> visited;
vector<int> depth;
vector<int> parents;

void dfs(int vertex) {
    visited[vertex] = true;
    if (!visited[parents[vertex]]) {
        dfs(parents[vertex]);
    }
    depth[vertex] = depth[parents[vertex]] + 1;
}

int log(int x) {
    int res = -1;
    while (x > 0) {
        x = x >> 1;
        res++;
    }
    return res;
}

int main() {
    int a, n, root;

    cin >> n;

    int upBound = log(n) + 1;

    depth.resize(n + 1, 0);
    parents.resize(n + 1, 0);
    visited.resize(n + 1, false);
    vector<vector<int>> dp(n + 1, vector<int>(upBound, 0));

    for (int i = 1; i <= n; i++) {
        cin >> a;
        if (a == 0) {
            root = i;
            parents[i] = i;
            continue;
        }
        parents[i] = a;
    }

    for (int i = 1; i <= n; i++) {
        if (!visited[i]) {
            dfs(i);
        }
    }

    for (int i = 1; i <= n; i++) {
        dp[i][0] = parents[i];
    }

    for (int j = 1, pow = 2; j < upBound; j++) {
        for (int i = 1; i <= n; i++) {
            if (pow < depth[i]) {
                dp[i][j] = dp[dp[i][j - 1]][j - 1];
            }
        }
        pow *= 2;
    }


    for (int i = 1; i <= n; i++) {
        cout << i << ": ";
        if (i != root) {
            for (int j = 0; j < upBound; j++) {
                if (dp[i][j] > 0) {
                    cout << dp[i][j] << " ";
                }
            }
        }
        cout << endl;
    }

    return 0;
}