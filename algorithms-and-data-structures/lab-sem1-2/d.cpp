#include <iostream>
#include <deque>

using namespace std;

void balance(deque<int> &d1, deque<int> &d2) {
    int d1s = d1.size();
    int d2s = d2.size();

    if (d2s > d1s) {
        d1.push_back(d2.front());
        d2.pop_front();
    } else if (d1s - d2s > 1) {
        d2.push_front(d1.back());
        d1.pop_back();
    }
}

void Plus(deque<int> &d1, deque<int> &d2, const int &id) {
    d2.push_back(id);

    balance(d1, d2);
}

void Multiple(deque<int> &d1, deque<int> &d2, const int &id) {
    d1.push_back(id);

    balance(d1, d2);
}

void Minus(deque<int> &d1, deque<int> &d2) {
    cout << d1.front() << endl;
    d1.pop_front();

    balance(d1, d2);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    deque<int> d1;
    deque<int> d2;

    int n;
    cin >> n;

    for (int i = 0; i < n; ++i) {
        char c;
        cin >> c;
        int id;
        switch (c) {
            case '+':
                cin >> id;
                Plus(d1, d2, id);
                break;
            case '*':
                cin >> id;
                Multiple(d1, d2, id);
                break;
            case '-':
                Minus(d1, d2);
                break;
        }
    }

    return 0;
}