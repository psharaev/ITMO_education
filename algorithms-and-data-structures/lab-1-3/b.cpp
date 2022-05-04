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

    ll n, m;
    cin >> n >> m;

    vector<vector<ll>> arr(n, vector<ll>(m));
    vector<vector<ll>> dp(n, vector<ll>(m, LONG_LONG_MIN));
    vector<vector<char>> parent(n, vector<char>(m, 'U'));
    for (ll i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            cin >> arr[i][j];
        }
    }
    dp[0][0] = arr[0][0];

    for (ll i = 0; i < n; ++i) {
        for (ll j = 0; j < m; ++j) {
            if (i > 0 && dp[i][j] < dp[i - 1][j] + arr[i][j]) {
                dp[i][j] = dp[i - 1][j] + arr[i][j];
                parent[i][j] = 'D';
            }
            if (j > 0 && dp[i][j] < dp[i][j - 1] + arr[i][j]) {
                dp[i][j] = dp[i][j - 1] + arr[i][j];
                parent[i][j] = 'R';
            }
        }
    }

    cout << dp.back().back() << endl;
    stack<char> path;
    ll i = n - 1, j = m - 1;
    while (i > 0 || j > 0) {
        path.push(parent[i][j]);
        if (parent[i][j] == 'R') {
            j--;
        } else if (parent[i][j] == 'D') {
            i--;
        }
    }

    while (!path.empty()) {
        cout << path.top();
        path.pop();
    }

    return 0;
}
