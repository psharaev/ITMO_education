#include <iostream>
#include <stack>

using namespace std;

struct meta_t {
    int number;
    int quantity;
};

void collapse(stack<meta_t> &s, int &ans) {
    while (!s.empty() && s.top().quantity >= 3) {
        ans += s.top().quantity;
        int num = s.top().number;
        while (!s.empty() && s.top().number == num) {
            s.pop();
        }
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n, ans = 0;
    cin >> n;

    stack<meta_t> s;

    for (int i = 0; i < n; ++i) {
        int x;
        cin >> x;

        if (s.empty()) {
            s.push({x, 1});
            continue;
        }
        
        if (s.top().number == x) {
            s.top().quantity++;
        } else {

            collapse(s, ans);

            if (!s.empty() && s.top().number == x)
                s.top().quantity++;
            else
                s.push({x, 1});

        }
    }

    collapse(s, ans);

    cout << ans << endl;

    return 0;
}