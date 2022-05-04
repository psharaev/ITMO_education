#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace std;

typedef long long ll;

typedef pair<ll, ll> point_t;
typedef vector<point_t> graph_t;
typedef vector<ll> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef vector<vb> vvb;
typedef vector<double> vd;

graph_t g;
vi d;
vb used;

ll distanceSquared(const point_t &a, const point_t &b) {
    ll first = (a.first - b.first);
    ll second = (a.second - b.second);
    return first * first + second * second;
}

inline ll distanceSquared(const ll &u, const ll &v) {
    return distanceSquared(g[u], g[v]);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    ll n;
    cin >> n;

    d.resize(n);
    used.resize(n, false);
    g.resize(n);

    for (ll i = 0; i < n; ++i) {
        ll x, y;
        cin >> x >> y;
        g.emplace(g.begin() + i, x, y);
    }

    used[0] = true;
    d[0] = -1;

    for (ll i = 1; i < n; ++i) {
        d[i] = distanceSquared(0, i);
    }

    double ans = 0;

    for (ll i = 0; i < n - 1; ++i) {
        ll v = -1;
        for (ll j = 0; j < n; ++j) {
            if (!used[j]) {
                v = j;
                break;
            }
        }

        for (ll j = v + 1; j < n; ++j) {
            if (!used[j] && d[v] > d[j]) {
                v = j;
            }
        }

        ans += sqrt(d[v]);
        used[v] = true;

        for (ll u = 0; u < n; ++u) {
            if (!used[u] && d[u] > distanceSquared(max(u, v), min(u, v))) {
                d[u] = distanceSquared(max(u, v), min(u, v));
            }
        }
    }

    cout << ans << endl;

    return 0;
}