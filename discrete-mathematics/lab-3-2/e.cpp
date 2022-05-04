#include <iostream>
#include <queue>
#include <vector>
#include <algorithm>

using namespace std;

typedef long long ll;

struct item_t {
    int w, id;
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    freopen("cycles.in", "r", stdin);
    freopen("cycles.out", "w", stdout);

    int n, m;
    cin >> n >> m;

    vector<bool> used(1ull << n, false);
    vector<item_t> items(n);
    queue<int> q;

    for (int i = 0; i < n; ++i) {
        cin >> items[i].w;
        items[i].id = i;
    }

    for (int i = 0; i < m; ++i) {
        int set = 0, k;

        cin >> k;
        for (int j = 0; j < k; ++j) {
            int x;
            cin >> x;
            x--;
            set |= 1 << x;
        }
        used[set] = true;
        q.push(set);
    }

    sort(items.rbegin(), items.rend(), [](const item_t &a, const item_t &b) {
        return a.w < b.w;
    });

    while (!q.empty()) {
        int set = q.front();
        q.pop();
        for (int i = 0; i < n; ++i) {
            int item = 1 << i;
            if ((set & item) == 0) {
                int setWithItem = set | item;
                if (!used[setWithItem]) {
                    used[setWithItem] = true;
                    q.push(setWithItem);
                }
            }
        }
    }

    ll curBase = 0, res = 0;
    for (int i = 0; i < n; ++i) {
        int item = 1 << items[i].id;
        if (!used[curBase | item]) {
            curBase |= item;
            res += items[i].w;
        }
    }

    cout << res << '\n';

    return 0;
}