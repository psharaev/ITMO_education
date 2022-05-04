#include <iostream>
#include <vector>
#include <set>
#include <algorithm>

using namespace std;

typedef long long ll;
typedef pair<ll, ll> pi;
typedef vector<ll> vi;
typedef vector<pi> vpi;
typedef set<ll> si;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    freopen("schedule.in", "r", stdin);
    freopen("schedule.out", "w", stdout);

    ll n;
    cin >> n;

    vpi a;
    si time;

    for (ll i = 0; i < n; ++i) {
        ll d, w;
        cin >> d >> w;
        a.emplace_back(w, d);
        time.insert(i + 1);
    }

    sort(a.rbegin(), a.rend());
    ll res = 0LL;

    for (pi task: a) {
        auto deadline = time.upper_bound(task.second);
        if (deadline == time.begin()) {
            res += task.first;
        } else {
            time.erase(--deadline);
        }
    }

    cout << res << '\n';

    return 0;
}
