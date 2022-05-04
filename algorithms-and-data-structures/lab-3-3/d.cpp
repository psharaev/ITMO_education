#include <iostream>
#include <vector>

using namespace std;

typedef unsigned long long ull;

typedef vector<ull> vi;

vector<ull> z_func(const string &s) {
    ull n = (ull) s.length();
    vector<ull> z(n);
    for (ull i = 1, l = 0, r = 0; i < n; ++i) {
        if (i <= r) {
            z[i] = min(r - i + 1, z[i - l]);
        }
        while (i + z[i] < n && s[z[i]] == s[i + z[i]]) {
            ++z[i];
        }
        if (i + z[i] - 1 > r) {
            l = i, r = i + z[i] - 1;
        }
    }
    return z;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    string s, t;
    cin >> s >> t;
    const ull s_len = s.length();
    vector<ull> z = std::move(z_func(s + "$" + t));

    vector<ull> ans;
    for (size_t i = s_len; i < z.size(); ++i) {
        if (z[i] == s_len) {
            ans.push_back(i - s_len);
        }
    }

    cout << ans.size() << '\n';
    for (const ull& item: ans) {
        cout << item << ' ';
    }

    return 0;
}