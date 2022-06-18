#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

typedef long long ll;

typedef vector<ll> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef vector<vb> vvb;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    ll n;
    cin >> n;

    vvb g(n, vb(n, false));

    for (ll i = 1; i < n; ++i) {
        for (ll j = 0; j < i; ++j) {
            char x;
            cin >> x;
            (x == '1' ? g[i][j] : g[j][i]) = true;
        }
    }

    vi p(1, 0);
    for (ll i = 1; i < n; ++i) {
        auto f = p.begin();
        while (f != p.end() && g[*f][i]) {
            ++f;
        }
        p.insert(f, i);
    }

    ll start = p[0], finish = 0;
    for (ll i = (ll) p.size() - 1; i >= 2; --i) {
        if (g[p[i]][start]) {
            finish = i;
            break;
        }
    }

    vi c(p.begin(), p.begin() + finish + 1);
    p.erase(p.begin(), p.begin() + finish + 1);

    auto startIt = p.begin();
    while (startIt != p.end()) {

        auto point = c.begin();
        while (point != c.end() && g[*point][*startIt]) {
            point++;
        }

        if (point == c.end()) {
            startIt++;
        } else {
            c.insert(point, p.begin(), startIt + 1);
            p.erase(p.begin(), startIt + 1);
            startIt = p.begin();
        }
    }

    for (const ll &i: c) {
        cout << i + 1 << ' ';
    }

    return 0;
}