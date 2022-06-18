#include <iostream>
#include <vector>

using namespace std;

class dsu {
private:
    vector<int> parent;
    vector<int> experience;
    vector<int> rang;
    int n;

public:
    dsu(const int &n) {
        this->n = n;
        parent = vector<int>(n);
        experience = vector<int>(n, 0);
        rang = vector<int>(n, 0);
        for (int i = 0; i < n; ++i) {
            parent[i] = i;
        }
    }

    const int &findRoot(const int &item) {
        return parent[item] == item ? item : findRoot(parent[item]);
    }

    void unionItems(const int &a, const int &b) {
        int x = findRoot(a);
        int y = findRoot(b);

        if (x == y) return;

        if (rang[x] == rang[y])
            rang[x]++;

        if (rang[x] < rang[y]) {
            parent[x] = y;
            experience[x] -= experience[y];
        } else {
            parent[y] = x;
            experience[y] -= experience[x];
        }
    }

    void addExp(const int &X, const int &Exp) {
        int root = findRoot(X);
        experience[root] += Exp;
    }

    int getExp(const int &X) {
        return parent[X] == X ? experience[X] : getExp(parent[X]) + experience[X];
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n, m;
    cin >> n >> m;

    dsu d(n + 1);

    for (int i = 0; i < m; ++i) {
        string func;
        cin >> func;
        if (func == "join") {
            int X, Y;
            cin >> X >> Y;
            d.unionItems(X, Y);
        } else if (func == "add") {
            int X, V;
            cin >> X >> V;
            d.addExp(X, V);
        } else if (func == "get") {
            int X;
            cin >> X;
            cout << d.getExp(X) << endl;
        } else {
            //cout << "lol ti kak suda popal?" << endl;
            break;
        }
    }

    return 0;
}