#include <iostream>
#include <algorithm>
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

ll find(vi &dsu, ll v) {
    return (v == dsu[v]) ? v : (dsu[v] = find(dsu, dsu[v]));
}

void dsu_union(vi &dsu, ll a, ll b) {
    a = find(dsu, a);
    b = find(dsu, b);
    if ((rand() & 1) > 0) {
        swap(a, b);
    }
    if (a != b) {
        dsu[a] = b;
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    ll n, m;
    cin >> n >> m;
    vi dsu(n);
    vector<pair<long long, pair<int, int>>> graph;

    for (ll i = 0; i < m; ++i) {
        ll u, v, w;
        cin >> u >> v >> w;
        graph.push_back({w, {u - 1, v - 1}});
    }

    sort(graph.begin(), graph.end());

    ll ans = 0;
    for (ll i = 0; i < n; ++i) {
        dsu[i] = i;
    }

    for (const auto &i: graph) {
        if (find(dsu, i.second.first) != find(dsu, i.second.second)) {
            ans += i.first;
            dsu_union(dsu, i.second.first, i.second.second);
        }
    }

    cout << ans << endl;

    return 0;
}
