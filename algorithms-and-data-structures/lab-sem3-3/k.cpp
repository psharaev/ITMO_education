#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

using namespace std;

typedef long long ll;
typedef vector<ll> vi;

struct suffix_t {
    ll id;
    ll r0, r1;
};

ll n;

bool cmp(const suffix_t &a, const suffix_t &b) {
    return a.r0 == b.r0 ? a.r1 < b.r1 : a.r0 < b.r0;
}

vector<suffix_t> buildSuffixArray(const string &s) {
    vector<suffix_t> res(n);
    vi ind(n);

    for (ll i = 0; i < n; i++) {
        res[i] = {i, s[i] - 'a', i + 1 < n ? s[i + 1] - 'a' : -1};
    }

    sort(res.begin(), res.end(), cmp);

    for (ll k = 4; k < 2 * n; k *= 2) {
        ll r = 0;
        ll last_r = res[0].r0;
        res[0].r0 = r;
        ind[res[0].id] = 0;

        for (ll i = 1; i < n; i++) {
            if (res[i].r0 == last_r && res[i].r1 == res[i - 1].r1) {
                last_r = res[i].r0;
                res[i].r0 = r;
            } else {
                last_r = res[i].r0;
                res[i].r0 = ++r;
            }
            ind[res[i].id] = i;
        }

        for (ll i = 0; i < n; i++) {
            ll next = res[i].id + k / 2;
            res[i].r1 = (next < n ? res[ind[next]].r0 : -1);
        }

        sort(res.begin(), res.end(), cmp);
    }

    return res;
}

vi buildLcp(const string &s, const vector<suffix_t> &suffixArr) {
    vi res(n, 0), ind(n, 0);

    for (ll i = 0; i < n; i++) {
        ind[suffixArr[i].id] = i;
    }

    ll k = 0;

    for (ll i = 0; i < n; i++) {
        if (ind[i] == n - 1) {
            k = 0;
            continue;
        }

        ll j = suffixArr[ind[i] + 1].id;

        while (i + k < n && j + k < n && s[i + k] == s[j + k]) {
            k++;
        }

        res[ind[i]] = k;
        if (k > 0) {
            k--;
        }
    }

    return res;
}


int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    string s;
    cin >> s;

    n = (ll) s.length();
    vector<suffix_t> suff = move(buildSuffixArray(s));
    vi lcp = move(buildLcp(s, suff));

    ll res = n - suff[0].id;

    for (ll i = 1; i < lcp.size(); i++) {
        res += n - suff[i].id - lcp[i - 1];
    }

    cout << res << '\n';
    return 0;
}

