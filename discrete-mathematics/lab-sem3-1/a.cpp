#include <iostream>
#include <vector>
#include <algorithm>
#include <deque>

using namespace std;

typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef vector<vb> vvb;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n;
    cin >> n;

    vvb g(n, vb(n, false));

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < i; ++j) {
            char x;
            cin >> x;
            if (x == '1') {
                g[j][i] = g[i][j] = true;
            }
        }
    }

    deque<int> q;
    for (int i = 0; i < n; ++i) {
        q.push_back(i);
    }

    for (int k = 0; k < n * (n - 1); ++k) {
        if (!g[q[0]][q[1]]) {
            int i = 2;
            while (!g[q[0]][q[i]] || !g[q[1]][i + 1]) {
                i++;
            }
            reverse(q.begin() + 1, q.begin() + i + 1);
        }
        q.push_back(q.front());
        q.pop_front();
    }

    while (!q.empty()) {
        cout << q.front() + 1 << ' ';
        q.pop_front();
    }

    return 0;
}