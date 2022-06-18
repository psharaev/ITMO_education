#include <iostream>
#include <string>
#include <vector>
#include <stack>
#include <deque>
#include <map>

using namespace std;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    deque<int> d;
    map<int, int> boost4;

    int n;
    cin >> n;

    for (int i = 0; i < n; ++i) {
        int func;
        cin >> func;

        switch (func) {
            case 1:
                int id;
                cin >> id;

                boost4[id] = d.size();

                d.push_back(id);
                break;
            case 2:
                boost4.erase(d.front());
                d.pop_front();

                for (auto &item : boost4) {
                    --item.second;
                }
                break;
            case 3:
                boost4.erase(d.back());
                d.pop_back();
                break;
            case 4:
                int q, counter;
                cin >> q;
                cout << boost4[q] << endl;

                break;
            case 5:
                cout << d[0] << endl;
                break;
        }
    }

    return 0;
}