#include <iostream>
#include <cmath>

using namespace std;

double getTime(const double &x, const double &Vp, const double &Vf, const double &a) {
    double hypotP = hypot(x, 1. - a);
    double hypotF = hypot(1. - x, a);

    return hypotP / Vp + hypotF / Vf;
}

int main() {
    double Vp, Vf;
    cin >> Vp >> Vf;

    double a;
    cin >> a;

    double l = 0.f, r = 1.f;
    double eps = 10e-6;

    while (r - l > eps) {
        double m1 = (l * 2. + r) / 3.;
        double m2 = (l + r * 2.) / 3.;
        if (getTime(m1, Vp, Vf, a) < getTime(m2, Vp, Vf, a))
            r = m2;
        else
            l = m1;

    }

    cout << (l + r) / 2.;
}
