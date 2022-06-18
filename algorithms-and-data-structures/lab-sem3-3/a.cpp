#include <iostream>
#include <vector>
#include <deque>

using namespace std;

typedef unsigned long long ull;

vector<ull> primes;
vector<ull> hashes;

const long long PRIME = 37;
string s;

uint64_t subHash(const ull &a, const ull &b) {
    return hashes[b + 1] - hashes[a] * primes[b - a + 1];
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    string s;
    ull m;
    cin >> s >> m;

    primes.resize(s.length() + 1);
    hashes.resize(s.length() + 1);

    primes[0] = 1;
    hashes[0] = 0;

    for (size_t i = 0; i < s.length(); i++) {
        primes[i + 1] = primes[i] * PRIME;
        hashes[i + 1] = hashes[i] * PRIME + s[i];
    }

    while (m-- > 0) {
        ull a, b, c, d;
        cin >> a >> b >> c >> d;
        cout << (subHash(--a, --b) == subHash(--c, --d) ? "Yes" : "No") << endl;
    }
    return 0;
}
