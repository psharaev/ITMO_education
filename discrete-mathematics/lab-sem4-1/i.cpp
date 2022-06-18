#include <iostream>
#include <utility>
#include <vector>

using namespace std;

typedef long long ll;
typedef vector<ll> vi;

#define MOD 104857601LL

vi vi_mul(const vi &p, const vi &q) {
    size_t n = p.size(), m = q.size();
    vi res(n + m - 1, 0);

    for (size_t i = 0; i < n; ++i) {
        for (size_t j = 0; j < m; ++j) {
            res[i + j] = (res[i + j] + ((p[i] + MOD) % MOD) * ((q[j] + MOD) % MOD)) % MOD;
        }
    }

    return res;
}

vector<ll> vi_neg(const vector<ll> &a) {
    vector<ll> res(a);
    for (size_t i = 1; i < a.size(); i += 2) {
        res[i] = -res[i];
    }
    return res;
}

struct recurrence {
    vector<ll> a;
    vector<ll> c;

    explicit recurrence(vi a, vi c) : a(move(a)), c(move(c)) {}

    [[nodiscard]] size_t getK() const {
        return a.size();
    }

    [[nodiscard]] ll getItem(size_t n) const {
        size_t k = getK();
        vector<ll> p(k);
        for (size_t i = 0; i < k; i++) {
            ll cur = 0ll;
            for (size_t j = 0; j < i; j++) {
                cur = (cur + a[i - j - 1] * -c[j + 1] + MOD) % MOD;
            }
            p[i] = (a[i] + MOD - cur) % MOD;
        }

        vi new_c(c);

        for (n--; n >= k; n /= 2ll) {
            vector<ll> neg_c = vi_neg(new_c);
            vector<ll> squar_c = vi_mul(new_c, neg_c);
            for (size_t i = 0; i < squar_c.size(); i += 2) {
                new_c[i / 2] = squar_c[i];
            }
            vector<ll> p_neg_c = vi_mul(p, neg_c);
            for (size_t i = n % 2ll; i < p_neg_c.size(); i += 2) {
                p[i / 2] = p_neg_c[i];
            }
        }

        vector<ll> res(k);
        for (size_t i = 0; i < k; i++) {
            ll cur = 0ll;
            for (size_t j = 0; j < i; j++) {
                cur = (cur + res[i - j - 1] * -new_c[j + 1] % MOD + MOD) % MOD;
            }
            res[i] = (cur + p[i]) % MOD;
        }

        return res[n];
    }
};

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    size_t k;
    ll n;

    cin >> k >> n;

    vector<ll> a(k);
    for (size_t i = 0; i < k; i++) {
        cin >> a[i];
    }

    vector<ll> c(k + 1);
    for (size_t i = 1; i <= k; i++) {
        cin >> c[i];
        c[i] = -c[i];
    }
    c[0] = 1ll;

    recurrence r(a, c);
    cout << r.getItem(n) << '\n';

    return 0;
}
