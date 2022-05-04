#include <iostream>
#include <vector>
#include <algorithm>
#include <set>

using namespace std;

typedef unsigned int ui;

size_t n, m;
vector<vector<ui>> sets;

inline bool existSet(const ui &set) {
    ui sz = __builtin_popcount(set);
    return any_of(sets[sz].begin(), sets[sz].end(), [&set](const ui &_set) {
        return _set == set;
    });
}

bool existChangeElem(const ui &smallSet, const ui &bigSet) {
    set<ui> diff;

    for (ui bits = bigSet, index = 0; bits > 0; ++index, bits >>= 1u) {
        if ((bits & 1u) != 0) {
            diff.insert(index);
        }
    }

    for (ui bits = smallSet, index = 0; bits > 0; ++index, bits >>= 1u) {
        if ((bits & 1u) != 0) {
            diff.erase(index);
        }
    }

    return any_of(diff.begin(), diff.end(), [&smallSet](const ui& x){
        return existSet(smallSet | (1u << x));
    });
}

bool check_1() {
    return !sets[0].empty();
}

bool check_2() {
    for (size_t i = 1; i < sets.size(); ++i) {
        for (const ui &set: sets[i]) {
            for (ui subSet = set; subSet > 0; subSet = (subSet - 1) & set) {
                if (!existSet(subSet)) {
                    return false;
                }
            }
        }
    }
    return true;
}

bool check_3() {
    for (size_t it_small = 0; it_small < sets.size() - 1; ++it_small) {
        for (const ui &smallSet: sets[it_small]) {
            for (size_t it_big = it_small + 1; it_big < sets.size(); ++it_big) {
                for (const ui &bigSet: sets[it_big]) {
                    if (!existChangeElem(smallSet, bigSet)) {
                        return false;
                    }
                }
            }
        }
    }
    return true;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    freopen("check.in", "r", stdin);
    freopen("check.out", "w", stdout);

    cin >> n >> m;
    sets.resize(n + 1);

    for (size_t i = 0; i < m; ++i) {
        size_t k;
        cin >> k;
        ui set = 0u;
        for (size_t j = 0; j < k; ++j) {
            ui x;
            cin >> x;
            x--;
            set |= (1u << x);
        }
        sets[k].push_back(set);
    }

    cout << (check_1() && check_2() && check_3() ? "YES" : "NO") << '\n';

    return 0;
}
