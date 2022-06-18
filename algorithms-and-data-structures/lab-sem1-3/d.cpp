#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <stack>

using namespace std;

typedef long long ll;
ll mod = 1000000000;

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

/*   0 1 2
 * 0 1 2 3
 * 1 4 5 6
 * 2 7 8 9
 * 3   0
 *
 */

vector<vector<ll>> nextKeyboard(const vector<vector<ll>> &last) {
    vector<vector<ll>> res(4, vector<ll>(3, 0));

    /* 1 */ res[0][0] = (last[1][2] + last[2][1]) % mod;
    /* 2 */ res[0][1] = (last[2][0] + last[2][2]) % mod;
    /* 3 */ res[0][2] = (last[1][0] + last[2][1]) % mod;
    /* 4 */ res[1][0] = (last[0][2] + last[2][2] + last[3][1]) % mod;
    /* 5 */ // no exist
    /* 6 */ res[1][2] = (last[0][0] + last[2][0] + last[3][1]) % mod;
    /* 7 */ res[2][0] = (last[0][1] + last[1][2]) % mod;
    /* 8 */ res[2][1] = (last[0][0] + last[0][2]) % mod;
    /* 9 */ res[2][2] = (last[0][1] + last[1][0]) % mod;
    /* 0 */ res[3][1] = (last[1][0] + last[1][2]) % mod;


    return res;
}

int main() {

    ll n;
    cin >> n;

    vector<vector<ll>> keyboard(4, vector<ll>(3, 1));
    keyboard[2][1] = 0;
    keyboard[3][0] = 0;
    keyboard[3][1] = 0;
    keyboard[3][2] = 0;

    for (int i = 1; i < n; ++i) {
        keyboard = nextKeyboard(keyboard);
    }

    dump(sum(keyboard) % mod);

    return 0;
}