#include <iostream>
#include <cmath>

using namespace std;

double f(const double &x) {
    return x * x + sqrt(x);
}

int main() {
    double c;
    cin >> c;

    double l = 0., r = 10e10;
    for (int i = 0; i < 100000; ++i) {
        double m = (l + r) / 2.;
        if (f(m) < c)
            l = m;
        else
            r = m;
    }

    cout.precision(12);
    cout << r;

    return 0;
}
