#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>

using namespace std;

typedef long long ll;

struct vertex_t {
    ll go[26]{};
    vector<ll> terms;

    ll up_link{0};
    ll s_link{0};
};

vector<vertex_t> tree;
ll cur_size;

void build() {
    queue<ll> q;

    q.push(0);

    while (!q.empty()) {
        ll cur = q.front();
        q.pop();

        for (ll i = 0; i < 26; ++i) {
            if (tree[cur].go[i] == 0) {
                tree[cur].go[i] = tree[tree[cur].up_link].go[i];
            } else {
                ll next = tree[cur].go[i];

                if (cur != 0) {
                    tree[next].up_link = tree[tree[cur].up_link].go[i];
                    if (tree[tree[next].up_link].terms.empty()) {
                        tree[next].s_link = tree[tree[next].up_link].s_link;
                    } else {
                        tree[next].s_link = tree[next].up_link;
                    }
                }

                q.push(next);
            }
        }
    }
}

vector<bool> findAns(const string &t) {
    vector<bool> res(cur_size);
    vector<bool> found(tree.size());

    ll cur = 0;
    for (const char &c: t) {
        cur = tree[cur].go[c - 'a'];

        ll tmp = cur;

        while (!found[tmp]) {
            if (!tree[tmp].terms.empty()) {
                for_each(tree[tmp].terms.begin(), tree[tmp].terms.end(),
                         [&res](const int &ends) { res[ends] = true; });
            }

            found[tmp] = true;
            tmp = tree[tmp].s_link;
        }
    }

    return res;
}


int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    ll n;
    cin >> n;

    string t;
    tree.emplace_back();
    cur_size = 0;

    for (ll i = 0; i < n; ++i) {
        string s;
        cin >> s;
        ll cur = 0;

        for (const char &c: s) {
            ll nextC = c - 'a';

            if (!tree[cur].go[nextC]) {
                tree.emplace_back();
                tree[cur].go[nextC] = (ll) tree.size() - 1;
            }

            cur = tree[cur].go[nextC];
        }

        tree[cur].terms.push_back(cur_size++);
    }

    build();

    cin >> t;

    vector<bool> ans = move(findAns(t));
    for_each(ans.begin(), ans.end(),
             [](const bool &i) { cout << (i ? "YES" : "NO") << '\n'; });

    return 0;
}