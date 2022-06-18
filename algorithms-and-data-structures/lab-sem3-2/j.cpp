#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>

using namespace std;

typedef long long ll;

typedef vector<ll> vi;
typedef vector<vi> vvi;

vi topSort(const vvi &g, vi &cnt_in) {
    vi res;
    queue<ll> q;
    for (ll i = 0; i < cnt_in.size(); ++i) {
        if (cnt_in[i] == 0) {
            q.push(i);
        }
    }

    while (!q.empty()) {
        ll from = q.front();
        q.pop();
        for (const ll to: g[from]) {
            if (--cnt_in[to] == 0) {
                q.push(to);
            }
        }

        res.push_back(from);
    }

    reverse(res.begin(), res.end());

    return res;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    ll n, m;
    cin >> n >> m;
    vvi g(n);
    vector<ll> cnt_in(n, 0);
    for (ll i = 0; i < m; ++i) {
        ll u, v;
        cin >> u >> v;
        g[u - 1].push_back(v - 1);
        cnt_in[v - 1]++;
    }

    vi ts = move(topSort(g, cnt_in));

    vi res(n, 0);
    for (const ll from: ts) {
        vector<ll> used;

        for (const ll to: g[from]) {
            used.push_back(res[to]);
        }

        sort(used.begin(), used.end());

        ll minVal = 0;
        for (ll val: used) {
            if (val == minVal) {
                ++minVal;
            }
        }

        res[from] = minVal;
    }

    for (const ll i: res) {
        cout << i << '\n';
    }
}
