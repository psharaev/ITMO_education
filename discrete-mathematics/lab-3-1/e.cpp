#include <vector>
#include <algorithm>
#include <set>
#include <list>
#include <cstdio>

using namespace std;

typedef vector<int> vi;
typedef list<int> li;
typedef vector<li> vli;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef vector<vb> vvb;


int findId(const vi &a, int item) {
    for (int i = 0; i < a.size(); ++i) {
        if (a[i] == item) {
            return i;
        }
    }
    return -1;
}

int main() {
    int n;
    scanf("%i", &n);

    set<int> q;

    vvi g(n, vi());
    vector<int> g_size(n, 0);
    for (int i = 0; i < n - 1; ++i) {
        int a, b;
        scanf("%i%i", &a, &b);
        a--;
        b--;
        g[a].push_back(b);
        g_size[a]++;
        g[b].push_back(a);
        g_size[b]++;

    }

    for (int i = 0; i < n; ++i) {
        if (g[i].size() == 1) {
            q.insert(i);
        }
    }


    int id = 0;
    while (id < n - 2) {
        int min = *q.begin();
        q.erase(q.begin());

        int x = g[min].front();

        printf("%i ", x + 1);

        g_size[min] = 0;

        int index = findId(g[x],min);
        std::copy(g[x].begin() + index + 1, g[x].end(), g[x].begin() + index);
        g[x].pop_back();
//        g[x].erase(remove(g[x].begin(), g[x].end(), min), g[x].end());
        g_size[x]--;
        if (g_size[x] == 1) {
            q.insert(x);
        }
        id++;
    }

    return 0;
}