#include <iostream>
#include <vector>
#include <set>
#include <queue>
#include <deque>

using namespace std;

typedef long long ll;

typedef vector<ll> vi;
typedef vector<vi> vvi;

#define LONG_LONG_MAX INF

void dump(const vi &a) {
    for (const ll &item: a) {
        cout << item << ' ';
    }
}

void dump(const vvi &a) {
    for (const vi &row: a) {
        dump(row);
        cout << '\n';
    }
    cout.flush();
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    ll n;
    cin >> n;
    vvi d(n, vi(n)); // строка, столбец

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            cin >> d[i][j];
        }
    }

    for (int k = 0; k < n; ++k) {
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < n; ++j) {
                d[i][j] = min(d[i][j], d[i][k] + d[k][j]);
            }
        }
    }

    dump(d);

    return 0;
}
