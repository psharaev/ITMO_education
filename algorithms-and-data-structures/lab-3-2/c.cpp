#include <iostream>
#include <vector>
#include <algorithm>


using namespace std;

typedef long long ll;

#define INF (ll)1e9
#define NOEDGE 100000LL

struct edge_t {
    ll u, v, w;

    edge_t() = default;

    edge_t(const ll &u, const ll &v, const ll &w) : u(u), v(v), w(w) {}
};


int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    ll n;
    cin >> n;

    vector<edge_t> g;

    for (ll i = 0; i < n; ++i) {
        for (ll j = 0; j < n; ++j) {
            ll w;
            cin >> w;
            if (w != NOEDGE) {
                g.emplace_back(i, j, w);
            }
        }
    }

    vector<ll> d(n, INF);
    vector<ll> p(n, -1);
    ll x;
    d[0] = 0;

    for (ll i = 0; i < n; ++i) {
        x = -1;
        for (const auto &e: g) {
            if (d[e.v] > d[e.u] + e.w) {
                d[e.v] = max(-INF, d[e.u] + e.w);
                p[e.v] = e.u;
                x = e.v;
            }
        }
    }

    if (x == -1) {
        cout << "NO" << endl;
    } else {
        vector<ll> c;

        for (uint64_t i = 0; i < n; i++) {
            x = p[x];
        }

        ll first = x;
        c.push_back(x);

        while (first != p[x]) {
            x = p[x];
            c.push_back(x);
        }

        cout << "YES" << endl;
        cout << c.size() << endl;

        for (auto i = c.rbegin(); i != c.rend(); ++i) {
            cout << (*i) + 1 << ' ';
        }
    }

    return 0;
}