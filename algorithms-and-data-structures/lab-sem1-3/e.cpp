#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <stack>

using namespace std;

typedef long long ll;

ll levenshteinDistance(const string &s1, const string &s2);

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

ll levenshteinDistance(const string &s1, const string &s2) {
    ll n = s1.size(), m = s2.size();

    vector<vector<ll>> dp(n + 1, vector<ll>(m + 1, -1));

    ll deleteCost = 1, insertCost = 1, replaceCost = 1;

    for (int i = 0; i < n + 1; ++i) {
        for (int j = 0; j < m + 1; ++j) {
            if (i == 0 || j == 0) {
                dp[i][j] = i * deleteCost + j * insertCost;
            } else if (s1[i - 1] == s2[j - 1]) {
                dp[i][j] = dp[i - 1][j - 1];
            } else {
                dp[i][j] = min(
                        dp[i][j - 1] + insertCost,
                        dp[i - 1][j] + deleteCost,
                        dp[i - 1][j - 1] + replaceCost);
            }
        }
    }

    return dp.back().back();
}

int main() {

    string s1, s2;
    cin >> s1 >> s2;

    dump(levenshteinDistance(s1, s2));

    return 0;
}