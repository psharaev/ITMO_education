#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <stack>

using namespace std;

typedef long long ll;

template<typename T>
void dump(const T &a) {
    cout << a << endl;
}

template<typename T>
void dump(const vector<T> &a) {
    for (const T &item : a) {
        cout << item << ' ';
    }
    cout << endl;
}

template<typename T>
void dump(const vector<vector<T>> &a) {
    for (const vector<T> &item : a) {
        dump(item);
    }
}

int main() {

    ll n, k;
    cin >> n >> k;

    vector<ll> arr(n, 0);
    vector<ll> dp(n, LONG_LONG_MIN);
    dp[0] = 0;
    vector<ll> parent(n, -1);
    for (ll i = 1; i < n - 1; ++i) {
        cin >> arr[i];
    }

    for (ll i = 0; i < n; ++i) {
        for (ll j = i + 1, end = min(n, k + i + 1); j < end; ++j) {
            if (dp[j] <= dp[i] + arr[j]) {
                dp[j] = dp[i] + arr[j];
                parent[j] = i;
            }
        }
    }

    cout << dp.back() << endl;
    stack<ll> path;
    path.push(n);
    int cur = parent.back();
    while (cur != -1) {
        path.push(cur + 1);
        cur = parent[cur];
    }
    cout << path.size() - 1 << endl;

    while (!path.empty()) {
        cout << path.top() << ' ';
        path.pop();
    }

    return 0;
}
