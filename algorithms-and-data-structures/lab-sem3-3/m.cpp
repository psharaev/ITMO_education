#include <iostream>
#include <vector>
#include <map>

using namespace std;
typedef long long ll;

struct state_t {
    int sz{}, href{};
    map<char, int> link;
};

state_t st[200002];
int sz, last;

void addChar(const char c) {
    int cur = sz++;
    st[cur].sz = st[last].sz + 1;
    int p;
    for (p = last; p != -1 && !st[p].link.count(c); p = st[p].href) {
        st[p].link[c] = cur;
    }
    if (p == -1) {
        st[cur].href = 0;
    } else {
        int q = st[p].link[c];
        if (st[p].sz + 1 == st[q].sz) {
            st[cur].href = q;
        } else {
            int clone = sz++;
            st[clone].sz = st[p].sz + 1;
            st[clone].link = st[q].link;
            st[clone].href = st[q].href;
            for (; p != -1 && st[p].link[c] == q; p = st[p].href) {
                st[p].link[c] = clone;
            }
            st[q].href = st[cur].href = clone;
        }
    }
    last = cur;
}

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    string s, t;
    cin >> s >> t;

    sz = last = 0;
    st[0].sz = 0;
    st[0].href = -1;
    ++sz;

    for (char c: s) {
        addChar(c);
    }

    int v = 0, l = 0, len = 0, bestRes = 0;
    for (int i = 0; i < (int) t.length(); ++i) {
        while (v && !st[v].link.count(t[i])) {
            v = st[v].href;
            l = st[v].sz;
        }
        if (st[v].link.count(t[i])) {
            v = st[v].link[t[i]];
            ++l;
        }
        if (l > len) {
            len = l, bestRes = i;
        }
    }

    cout << t.substr(bestRes - len + 1, len) << '\n';
    return 0;
}