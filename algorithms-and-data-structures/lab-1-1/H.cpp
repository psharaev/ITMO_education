#include <iostream>

typedef long long int lli;
using namespace std;

lli w, h, n;

lli Check(lli a) {
    return (a / w) * (a / h) >= n;
}

lli min(lli a, lli b) {
    return a < b ? a : b;
}

lli max(lli a, lli b) {
    return a > b ? a : b;
}

int main() {
    cin >> w >> h >> n;
    lli l = min(w, h);
    lli r = n * max(w, h);
    lli m;
    while (r - l > 1) {
        m = (l + r) / 2;
        if (Check(m))
            r = m;
        else
            l = m;
    }
    if (Check(l))
        cout << l;
    else
        cout << r;
    return 0;
}
