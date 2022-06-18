#include <iostream>
#include <string>
#include <vector>
#include <stack>

using namespace std;

int topPop(stack<int> &s) {
    int res = s.top();
    s.pop();
    return res;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    stack<int> numbers;

    string s;
    while (cin >> s) {
        if (s == "+") {
            numbers.push(topPop(numbers) + topPop(numbers));
        } else if (s == "-") {
            int b = numbers.top();
            numbers.pop();
            int a = numbers.top();
            numbers.pop();

            numbers.push(a - b);
        } else if (s == "*") {
            numbers.push(topPop(numbers) * topPop(numbers));
        } else {
            numbers.push(stoi(s));
        }
    }

    cout << numbers.top() << endl;
    
    return 0;
}