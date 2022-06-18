#include <iostream>
#include <vector>
#include <algorithm>
#include <stack>

using namespace std;

typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef vector<vb> vvb;

void topologicalSortUtil(const vvi &g, const int &v, vb &used, stack<int> &s) {
    used[v] = true;

    for (const auto &i: g[v]) {
        if (!used[i]) {
            topologicalSortUtil(g, i, used, s);
        }
    }

    s.push(v);
}

void topologicalSort(const vvi &g, const int &n) {
    stack<int> s;

    vb used(n, false);
    for (int i = 0; i < n; i++) {
        if (!used[i]) {
            topologicalSortUtil(g, i, used, s);
        }
    }

    while (!s.empty()) {
        cout << s.top() + 1 << ' ';
        s.pop();
    }
}

bool isCyclicUtil(const vvi &g, const int &v, vb &visited, vb &recStack) {
    if (!visited[v]) {
        visited[v] = true;
        recStack[v] = true;

        for (const auto &i: g[v]) {
            if (!visited[i] && isCyclicUtil(g, i, visited, recStack)) {
                return true;
            } else if (recStack[i]) {
                return true;
            }
        }
    }

    recStack[v] = false;
    return false;
}

bool isCyclic(const vvi &g, const int &n) {
    vb visited(n, false);
    vb recStack(n, false);

    for (int i = 0; i < n; i++) {
        if (isCyclicUtil(g, i, visited, recStack)) {
            return true;
        }
    }

    return false;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n, m;
    cin >> n >> m;

    vvi g(n, vi());

    for (int i = 0; i < m; ++i) {
        int u, v;
        cin >> u >> v;
        g[u - 1].push_back(v - 1);
    }


    if (!isCyclic(g, n)) {
        topologicalSort(g, n);
    } else {
        cout << -1 << '\n';
    }

    return 0;
}
