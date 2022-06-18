#include <iostream>
#include <vector>
#include <stack>
#include <algorithm>

using namespace std;

#define PUSH true
#define POP false

int pop(stack<int> &s, vector<bool> &v) {
    int res = s.top();
    s.pop();
    v.push_back(POP);
    return res;
}

void push(stack<int> &s, vector<bool> &v, const int &val) {
    s.push(val);
    v.push_back(PUSH);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n;
    cin >> n;

    stack<int> s;
    vector<bool> ans;

    bool isFirstPop = true;

    int minItem;
    int firstPopItem;
    int lastPopItem;

    for (int i = 0; i < n; ++i) {
        int x;
        cin >> x;

        if (i == 0) {
            minItem = x;
        }

        minItem = min(minItem, x);
        while (!s.empty() && s.top() < x) {
            if (!isFirstPop && s.top() < lastPopItem) {
                cout << "impossible" << endl;
                return 0;
            }

            if (isFirstPop) {
                firstPopItem = s.top();
                isFirstPop = false;
            }

            lastPopItem = pop(s, ans);
        }

        push(s, ans, x);
    }

    if (!isFirstPop && minItem != firstPopItem) {
        cout << "impossible" << endl;
        return 0;
    }

    for (bool &&cmd : ans) {
        cout << (cmd == PUSH ? "push" : "pop") << endl;
    }

    for (int i = 0; i < s.size(); ++i) {
        cout << "pop" << endl;
    }

    return 0;
}