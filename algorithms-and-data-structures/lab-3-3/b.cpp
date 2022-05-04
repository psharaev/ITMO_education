#include <iostream>
#include <vector>

using namespace std;

typedef unsigned long long ull;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    string s;
    cin >> s;
    vector<ull> pi(s.length());
    pi[0] = 0;

    for (ull i = 1; i < s.length(); ++i) {
        ull k = pi[i - 1];
        while (k > 0) {
            if (s[k] == s[i]) {
                break;
            }
            k = pi[k - 1];
        }

        if (s[k] == s[i]) {
            k++;
        }

        pi[i] = k;
    }

    for (const ull p: pi) {
        cout << p << ' ';
    }

    return 0;
}