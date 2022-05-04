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

    ll n;
    cin >> n;

    vector<ll> a(n);
    for (int i = 0; i < n; ++i) {
        cin >> a[i];
    }

    vector<ll> d(n, 1);
    vector<ll> prev(n, -1);

    for (ll i = 0; i < n; i++) {
        for (ll j = 0; j < i; j++) {
            if ((a[i] > a[j] && d[i] < d[j] + 1)) {
                d[i] = d[j] + 1;
                prev[i] = j;
            }
        }
    }

    ll pos = 0;
    ll length = d[0];
    for (ll i = 0; i < n; i++) {
        if (d[i] > length) {
            pos = i;
            length = d[i];
        }
    }

    vector<int> answer;
    while (pos != -1) {
        answer.push_back(a[pos]);
        pos = prev[pos];
    }
    reverse(answer.begin(), answer.end());

    dump(answer.size());
    dump(answer);
    
    return 0;
}
