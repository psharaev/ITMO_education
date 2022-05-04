#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>

using namespace std;

typedef long long ll;

typedef vector<ll> vi;
typedef vector<vi> vvi;

ll n, m;

vvi gT;
vector<bool> win;
vector<bool> loose;
vector<ll> cnt_out;

void bfs() {
    queue<ll> q;
    for (ll i = 0; i < n; ++i) {
        if (cnt_out[i] == 0) {
            loose[i] = true;
            q.push(i);
        }
    }

    while (!q.empty()) {
        ll u = q.front();
        q.pop();
        for (const ll v: gT[u]) {
            if (!loose[v] && !win[v]) {
                if (win[u] && --cnt_out[v] == 0) {
                    loose[v] = true;
                } else if (loose[u]) {
                    win[v] = true;
                } else {
                    continue;
                }

                q.push(v);
            }
        }
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    while (cin >> n >> m) {

        gT.assign(n, vi());
        win.assign(n, false);
        loose.assign(n, false);
        cnt_out.assign(n, 0);

        for (ll i = 0; i < m; ++i) {
            ll from, to;
            cin >> from >> to;
            gT[to - 1].push_back(from - 1);
            cnt_out[from - 1]++;
        }

        bfs();

        for (ll i = 0; i < n; ++i) {
            if (!win[i] && !loose[i]) {
                cout << "DRAW\n";
            } else if (win[i]) {
                cout << "FIRST\n";
            } else {
                cout << "SECOND\n";
            }
        }
    }
}
