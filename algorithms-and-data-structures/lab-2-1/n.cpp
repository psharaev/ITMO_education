#include <algorithm>
#include <vector>
#include <stdio.h>

using namespace std;

typedef long long ll;
typedef vector<long long> vll;
typedef vector<vll> vvll;
typedef vector<vvll> vvvll;

vvvll t;
int n, m;

ll sum(int x, int y, int z) {
    ll result = 0;
    for (int i = x; i >= 0; i = (i & (i + 1)) - 1)
        for (int j = y; j >= 0; j = (j & (j + 1)) - 1)
            for (int k = z; k >= 0; k = (k & (k + 1)) - 1)
                result += t[i][j][k];
    return result;
}

void inc(int x, int y, int z, const ll &delta) {
    for (int i = x; i < n; i = (i | (i + 1)))
        for (int j = y; j < n; j = (j | (j + 1)))
            for (int k = z; k < n; k = (k | (k + 1)))
                t[i][j][k] += delta;
}

ll sum(int x1, int y1, int z1, int x2, int y2, int z2) {
    return sum(x2, y2, z2)
           - sum(x1 - 1, y2, z2)
           - sum(x2, y1 - 1, z2)
           + sum(x1 - 1, y1 - 1, z2)
           - sum(x2, y2, z1 - 1)
           + sum(x1 - 1, y2, z1 - 1)
           + sum(x2, y1 - 1, z1 - 1)
           - sum(x1 - 1, y1 - 1, z1 - 1);
}

int main() {
    scanf("%i", &n);

    t = vvvll(n, vvll(n, vll(n, 0LL)));

    while (true) {
        scanf("%i", &m);
        if (m == 1) {
            int x, y, z;
            ll k;
            scanf("%i%i%i%lli", &x, &y, &z, &k);
            inc(x, y, z, k);
        } else if (m == 2) {
            int x1, y1, z1, x2, y2, z2;
            scanf("%i%i%i%i%i%i", &x1, &y1, &z1, &x2, &y2, &z2);
            printf("%lld\n", sum(x1, y1, z1, x2, y2, z2));
        } else {
            break;
        }
    }

    return 0;
}