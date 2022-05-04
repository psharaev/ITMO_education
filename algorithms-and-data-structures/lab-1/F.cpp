#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int findR(const vector<int> &v, const int &x) {
    int l = -1, r = v.size()-1;
    while (r - l > 1) {
        int m = (l + r) / 2;
        if (v[m] < x)
            l = m;
        else
            r = m;
    }
    return r;
}

int findL(const vector<int> &v, const int &x) {
    int l = 0, r = v.size();
    while (r - l > 1) {
        int m = (l + r) / 2;
        if (v[m] <= x)
            l = m;
        else
            r = m;
    }
    return l;
}

int main() {
    int n, k;
    cin >> n >> k;

    vector<int> arr(n);
    for (int i = 0; i < n; ++i) {
        cin >> arr[i];
    }

    for (int i = 0; i < k; ++i) {
        int x, l, r;
        cin >> x;
        l = findL(arr, x);
        r = findR(arr, x);
        //cout << l << ' ' << r << ' ';
        int res;
        if (r == n) res = arr[l];
        else if (l == -1) res = arr[r];
        else if (l == r) res = arr[l];
        else if (abs(x-arr[l]) <= abs(x-arr[r])) res = arr[l];
        else res = arr[r];
        cout << res << endl;
    }
}
