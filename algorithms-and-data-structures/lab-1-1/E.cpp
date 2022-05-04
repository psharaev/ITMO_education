#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int findL(const vector<int> &v, const int &x) {
    int l = -1, r = v.size();
    while (r - l > 1) {
        int m = (l + r) / 2;
        if (v[m] < x)
            l = m;
        else
            r = m;
    }
    return r;
}

int findR(const vector<int> &v, const int &x) {
    int l = -1, r = v.size();
    while (r - l > 1) {
        int m = (l + r) / 2;
        if (v[m] > x)
            r = m;
        else
            l = m;
    }
    return l;
}

int main() {
    int n;
    cin >> n;

    vector<int> arr(n);
    for (int i = 0; i < n; ++i) {
        cin >> arr[i];
    }

    sort(arr.begin(), arr.end());

    int k;
    cin >> k;

    for (int i = 0; i < k; ++i) {
        int l, r;
        cin >> l >> r;
        l = findL(arr, l);
        r = findR(arr, r);
        cout << (r - l) + 1 << ' ';
    }
}
