#include <iostream>
#include <string>
#include <utility>
#include <vector>
#include <algorithm>
#include <map>
#include <stack>
#include <deque>

using namespace std;

typedef long long ll;
typedef vector<vector<long long>> vvll;
typedef vector<long long> vll;

#define ll_MAX 10000000

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


#define inf INT_MAX;

ll calcDp2(vvll &dp, vll &a, const ll &i, const ll &j) {
    if (j > i) {
        return inf;
    } else {
        ll res;
        ll cost = a[i];
        if (j <= 0) {
            if (i >= 1) {
                if (cost <= 100) {
                    ll dif = min(calcDp2(dp, a, i - 1, j + 1), calcDp2(dp, a, i - 1, j) + cost);
                    res = dif;
                } else {
                    return calcDp2(dp, a, i - 1, j + 1);
                }
            } else {
                return 0L;
            }
        } else {
            if (dp[i][j] != -1) {
                return dp[i][j];
            }
            ll dif = min(calcDp2(dp, a, i - 1, j + 1), calcDp2(dp, a, i - 1, cost > 100 ? j - 1 : j) + cost);
            res = dif;
        }
        dp[i][j] = res;
        return res;
    }
}


void calcDp1(vvll &dp, vll &a, deque<ll> &used, const ll &i, const ll &j) {
    if (j < i) {
        ll cost = a[i];
        if (j <= 0L && i >= 1L) {
            if (cost > 100L) {
                used.push_back(i);
                calcDp1(dp, a, used, i - 1, j + 1);
            } else if (calcDp2(dp, a, i, j) == calcDp2(dp, a, i - 1, j + 1)) {
                used.push_back(i);
                calcDp1(dp, a, used, i - 1, j + 1);
            } else {
                calcDp1(dp, a, used, i - 1, j);
            }
        } else if (cost <= 100L) {
            if (calcDp2(dp, a, i - 1, j + 1) == calcDp2(dp, a, i, j)) {
                used.push_back(i);
                calcDp1(dp, a, used, i - 1, j + 1);
            } else {
                calcDp1(dp, a, used, i - 1, j);
            }
        } else if (calcDp2(dp, a, i - 1, j + 1) == calcDp2(dp, a, i, j)) {
            used.push_back(i);
            calcDp1(dp, a, used, i - 1, j + 1);
        } else {
            calcDp1(dp, a, used, i - 1, j - 1);
        }
    }
}

int main() {
    ll n, k1 = 0, k2 = 0, ans = inf;
    cin >> n;
    vll a = vll(n + 1, 0L);
    for (ll i = 1; i <= n; i++)
        cin >> a[i];

    vvll dp = vvll(n + 1, vll(n + 2, -1L));

    for (ll i = 0; i <= n; i++) {
        ll cash = calcDp2(dp, a, n, i);
        if (ans >= cash) {
            ans = cash;
            k1 = i;
        }
    }

    deque<ll> used;

    calcDp1(dp, a, used, n, k1);

    k2 = used.size();

    cout << ans << endl;
    cout << k1 << ' ' << k2 << endl;

    while (!used.empty()) {
        cout << used.back() << endl;
        used.pop_back();
    }

    return 0;
}