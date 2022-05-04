#include <deque>
#include <vector>
#include <iostream>
#include <algorithm>

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

    vvb g(n + 1, vb(n + 1, false));
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

    for (int k = 0; k < n * (n - 1); k++) {
        if (!g[q[0]][q[1]]) {
            int j = 2;
            while (j < n - 1 && (!g[q[0]][q[j]] || !g[q[1]][q[j + 1]])) {
                j++;
            }
            if (j == n - 1) {
                j = 2;
                while (j < n && !g[q[0]][q[j]]) {
                    j++;
                }
            }
            reverse(q.begin() + 1, q.begin() + j + 1);
        }
        q.push_back(q.front());
        q.pop_front();
    }

    for (const int &i: q) {
        cout << i + 1 << ' ';
    }

    return 0;
}