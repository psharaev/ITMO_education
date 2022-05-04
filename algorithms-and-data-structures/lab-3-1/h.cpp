#include <iostream>
#include <valarray>
#include <vector>

using namespace std;

typedef long long ll;

typedef pair<ll, ll> point_t;
typedef vector<point_t> graph_t;
typedef vector<ll> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef vector<vb> vvb;
typedef vector<double> vd;

vvi g;
vb used;
ll n, m;

bool isAllVisit() {
    for (ll i = 0; i < n; i++) {
        if (!used[i]) {
            return false;
        }
    }
    return true;
}

void dfs(const ll &v, const bool &isSwap) {
    used[v] = true;
    for (ll i = 0; i < n; i++) {
        if ((isSwap ? g[i][v] <= m : g[v][i] <= m) && !used[i]) {
            dfs(i, isSwap);
        }
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    cin >> n;

    g.resize(n, vi(n, 0));

    for (ll i = 0; i < n; i++) {
        for (ll j = 0; j < n; j++) {
            cin >> g[i][j];
        }
    }

    ll l = 0, r = 1000000000;
    while (l < r) {
        m = (l + r) / 2;

        used.assign(n, false);

        dfs(0, false);

        bool sol = false;
        if (isAllVisit()) {
            used.assign(n, false);
            dfs(0, true);
            if (!isAllVisit()) {
                sol = true;
            }
        } else {
            sol = true;
        }

        if (sol) {
            l = m + 1;
        } else {
            r = m;
        }

    }

    cout << r << endl;

    return 0;
}