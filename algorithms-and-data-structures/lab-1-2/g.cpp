#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

struct metaData_t {
    int minItem;
    int maxItem;
    int quantity;

    metaData_t(const int &minItem = 0, const int &maxItem = 0, const int &quantity = 0) {
        this->minItem = minItem;
        this->maxItem = maxItem;
        this->quantity = quantity;
    }
};

metaData_t combine(const metaData_t &a, const metaData_t &b) {
    return metaData_t(min(a.minItem, b.minItem),
                      max(a.maxItem, b.maxItem),
                      a.quantity + b.quantity);
}

void dump(const metaData_t &m) {
    cout << m.minItem << ' ' << m.maxItem << ' ' << m.quantity << endl;
}

class dsu {
private:
    vector<int> parent;
    vector<metaData_t> metaData;
    int n;

public:
    dsu(const int &n) {
        this->n = n;
        parent = vector<int>(n);
        metaData = vector<metaData_t>(n);
        for (int i = 0; i < n; ++i) {
            parent[i] = i;
            metaData[i] = metaData_t(i, i, 1);
        }
    }

    int findRoot(const int &item) {
        return parent[item] == item ? item : findRoot(parent[item]);
    }

    metaData_t getData(const int &item) {
        int root = findRoot(item);
        return metaData[root];
    }

    void unionItems(const int &a, const int &b) {
        int x = findRoot(a);
        int y = findRoot(b);
        if (x == y) return;
        parent[x] = y;
        metaData[y] = combine(metaData[x], metaData[y]);

//        for (int i = 1; i < n; ++i) {
//            cout << "i: " << i <<
//                 " parent: " << parent[i] <<
//                 " meta: " << metaData[i].minItem << ' ' << metaData[i].maxItem << ' ' << metaData[i].quantity << endl;
//        }
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n;
    cin >> n;

    dsu d(n + 1);

    string func;
    while (cin >> func) {
        if (func == "union") {
            int a, b;
            cin >> a >> b;
            d.unionItems(a, b);
        } else if (func == "get") {
            int x;
            cin >> x;
            dump(d.getData(x));
        } else {
            //cout << "lol ti kak suda popal?" << endl;
            break;
        }
    }

    return 0;
}