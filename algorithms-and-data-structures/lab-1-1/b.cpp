#include <iostream>
#include <vector>
using namespace std;

vector<int> sortCounting(const vector<int> &v, const int &maxNumber) {
    vector<int> counter(maxNumber + 1, 0);
    vector<int> res(v.size());

    for (int i = 0; i < v.size(); i++)
        counter[v[i]]++;

    int resPointer = 0;
    for (int i = 0; i < maxNumber + 1; i++)
        for (int j = 0; j < counter[i]; j++)
            res[resPointer++] = i;

    return res;
}

int main() {
    int n;
    cin >> n;

    vector<int> inp(n);
    for (int i = 0; i < n; i++)
        cin >> inp[i];

    vector<int> res = sortCounting(inp, 101);
    for (int i = 0; i < res.size(); i++)
        cout << res[i] << ' ';

    return 0;
}
