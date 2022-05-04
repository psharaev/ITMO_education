#include <iostream>
#include <string>
#include <utility>
#include <vector>
#include <algorithm>
#include <map>
#include <stack>

using namespace std;

typedef long long ll;
typedef vector<vector<long long>> vvll;
typedef vector<long long> vll;

#define INF 10000000

template<typename T>
T min(const T &a, const T &b, const T &c) {
    return min(min(a, b), c);
}

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

template<typename T>
T sum(const vector<T> &a) {
    T res = 0;
    for (const T &item : a) {
        res += item;
    }
    return res;
}

template<typename T>
T sum(const vector<vector<T>> &a) {
    T res = 0;
    for (const vector<T> &item : a) {
        res += sum(item);
    }
    return res;
}

bool check(ll x, ll y, ll n) {
    vll b(4);
    for (int i = 0; i < n - 1; i++) {
        b[0] = (x & (1 << i)) == 0 ? 0 : 1;
        b[1] = (x & (1 << (i + 1))) == 0 ? 0 : 1;
        b[2] = (y & (1 << i)) == 0 ? 0 : 1;
        b[3] = (y & (1 << (i + 1))) == 0 ? 0 : 1;
        if ((b[0] == b[1]) && (b[1] == b[2]) && (b[2] == b[3])) {
            return false;
        }
    }

    return true;
}

ll solve(ll n, ll m) {
    if (n > m) {
        swap(n, m);
    }
    
    ll res = 0;
    ll len = 1 << n;
    vvll a(m, vll(len));
    vvll dp(len, vll(len));

    for (ll i = 0; i < len; i++) {
        for (ll j = 0; j < len; j++) {
            dp[i][j] = check(i, j, n) ? 1 : 0;
        }
    }

    for (ll i = 0; i < len; i++) {
        a[0][i] = 1;
    }

    for (ll x = 1; x < m; x++) {
        for (ll i = 0; i < len; i++) {
            for (ll j = 0; j < len; j++) {
                a[x][i] = a[x][i] + a[x - 1][j] * dp[j][i];
            }
        }
    }

    for (ll i = 0; i < len; i++) {
        res += a[m - 1][i];
    }

    return res;
}

int main() {

    ll n, m;
    cin >> n >> m;

    dump(solve(n,m));

//    dump(solve(1, 1));
//    dump(solve(1, 2));

    return 0;
}