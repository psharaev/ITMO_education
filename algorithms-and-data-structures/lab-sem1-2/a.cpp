#include <iostream>
#include <vector>
#include <stack>
#include <algorithm>

using namespace std;

#define PUSH 1
#define POP 2
#define GET_MIN 3

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n;
    cin >> n;

    stack<int> data;
    stack<int> minInData;

    for (int i = 0; i < n; ++i) {
        int func;
        cin >> func;
        switch (func) {
            case PUSH:
                int x;
                cin >> x;
                data.push(x);
                minInData.push(minInData.empty() ? x : min(minInData.top(), x));
                break;
            case POP:
                data.pop();
                minInData.pop();
                break;
            case GET_MIN:
                cout << minInData.top() << endl;
                break;
        }
    }

    return 0;
}