#include <iostream>
#include <algorithm>
#include <vector>
#include <list>

using namespace std;

bool cmp(int i, int j) {
    cout << 1 << ' ' << i << ' ' << j << endl;
    string answer;
    cin >> answer;
    return answer[0] == 'Y';
}

int main() {
    int n;
    cin >> n;

    list<int> arr;
    for (int i = 0; i < n; ++i) {
        arr.push_back(i + 1);
    }

    arr.sort(cmp);

    cout << 0 << ' ';
    for (const int &i: arr) {
        cout << i << ' ';
    }

    return 0;
}