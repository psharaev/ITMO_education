#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

typedef long long ll;

typedef vector<ll> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;

ll n, m;
vvi g;
vi mt;
vb used;

bool try_kuhn(const ll &v) {
    if (used[v]) {
        return false;
    }
    used[v] = true;
    for (ll i = 0; i < g[v].size(); ++i) {
        ll to = g[v][i];
        if (mt[to] == -1 || try_kuhn(mt[to])) {
            mt[to] = v;
            return true;
        }
    }
    return false;
}

int main() {
    cin >> n >> m;
    g.resize(n);
    mt.assign(m, -1);

    for (ll i = 0; i < n; ++i) {
        ll v;
        while (true) {
            cin >> v;
            if (v == 0) {
                break;
            }
            g[i].push_back(v - 1);
        }
    }

    for (ll i = 0; i < n; ++i) {
        used.assign(n, false);
        try_kuhn(i);
    }

    cout << mt.size() - std::count(mt.begin(), mt.end(), -1) << '\n';

    for (ll i = 0; i < m; ++i) {
        if (mt[i] != -1) {
            cout << mt[i] + 1 << ' ' << i + 1 << '\n';
        }
    }

    return 0;
}