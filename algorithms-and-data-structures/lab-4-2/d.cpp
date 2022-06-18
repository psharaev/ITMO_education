#include <iostream>
#include <vector>
#include <queue>
#include <map>

using namespace std;

typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef queue<int> qi;

#define MAX_SIZE 10000

int n;

struct edge_t {
    int u, v, i, j, c, f = 0;

    edge_t(const int &u, const int &v, const int &i, const int &j, const int &c) : u(u), v(v), i(i), j(j), c(c) {}
};

vector<edge_t> edges;
vi d;

bool bfs(const vvi &g) {
    d.assign(MAX_SIZE, MAX_SIZE);
    qi q;
    d[0] = 0;
    q.push(0);
    while (!q.empty()) {
        const int v = q.front();
        q.pop();
        for (const int &index: g[v]) {
            const edge_t &uv = edges[index];
            if (uv.f < uv.c && d[uv.v] == MAX_SIZE) {
                d[uv.v] = d[v] + 1;
                q.push(uv.v);
            }
        }
    }
    return d[MAX_SIZE - 1] != MAX_SIZE;
}

int dfs(const vvi &g, vi &p, const int &v, const int &minCut) {
    if (v == (MAX_SIZE - 1) || minCut == 0) {
        return minCut;
    }

    while (p[v] < g[v].size()) {
        edge_t &uv = edges[g[v][p[v]]];
        if (d[uv.v] == d[v] + 1) {
            int delta = dfs(g, p, uv.v, min(minCut, uv.c - uv.f));
            if (delta != 0) {
                uv.f += delta;
                edges[g[v][p[v]] ^ 1].f -= delta;
                return delta;
            }
        }
        p[v]++;
    }

    return 0;
}

template<typename T>
void dump(const vector<T> &a) {
    for (const T &item: a) {
        cout << item;
    }
    cout << '\n';
}

template<typename T>
void dump(const vector<vector<T>> &a) {
    for (const vector<T> &item: a) {
        dump(item);
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);
    cin >> n;

    vvi g(MAX_SIZE);
    int count = n + 1;
    map<pair<int, int>, int> match;
    vector<vector<char>> tourney(n, vector<char>(n));
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            cin >> tourney[i][j];
            if (i < j && tourney[i][j] == '.') {
                match[{i, j}] = count++;
            }
        }
    }

    vi scores(n);
    for (int i = 0; i < n; ++i) {
        cin >> scores[i];
    }

    for (int i = 0; i < n; ++i) {
        for (int j = i + 1; j < n; ++j) {
            if (tourney[i][j] == 'W') {
                scores[i] -= 3;
            } else if (tourney[i][j] == 'w') {
                scores[i] -= 2;
                scores[j] -= 1;
            } else if (tourney[i][j] == 'L') {
                scores[j] -= 3;
            } else if (tourney[i][j] == 'l') {
                scores[i] -= 1;
                scores[j] -= 2;
            }
            if (tourney[i][j] != '.') {
                continue;
            }
            g[match[{i, j}]].push_back(edges.size());
            edges.emplace_back(match[{i, j}], i + 1, i, j, 3);
            g[i + 1].push_back(edges.size());
            edges.emplace_back(i + 1, match[{i, j}], MAX_SIZE, MAX_SIZE, 0);
            g[match[{i, j}]].push_back(edges.size());
            edges.emplace_back(match[{i, j}], j + 1, i, j, 3);
            g[j + 1].push_back(edges.size());
            edges.emplace_back(j + 1, match[{i, j}], MAX_SIZE, MAX_SIZE, 0);
        }
    }
    for (int i = n + 1; i < count; ++i) {
        g[0].push_back(edges.size());
        edges.emplace_back(0, i, MAX_SIZE, MAX_SIZE, 3);
        g[i].push_back(edges.size());
        edges.emplace_back(i, 0, MAX_SIZE, MAX_SIZE, 0);
    }
    for (int i = 1; i <= n; ++i) {
        g[i].push_back(edges.size());
        edges.emplace_back(i, MAX_SIZE - 1, MAX_SIZE, MAX_SIZE, scores[i - 1]);
        g[MAX_SIZE - 1].push_back(edges.size());
        edges.emplace_back(MAX_SIZE - 1, i, MAX_SIZE, MAX_SIZE, 0);
    }

    int maxFlow = 0;
    while (bfs(g)) {
        vi p(MAX_SIZE, 0);
        int delta;
        do {
            delta = dfs(g, p, 0, MAX_SIZE);
            maxFlow += delta;
        } while (delta != 0);
    }

    for (const edge_t &e: edges) {
        if (e.i < MAX_SIZE && e.j < MAX_SIZE) {
            if (tourney[e.i][e.j] == '.') {
                switch (e.f) {
                    case 3:
                        tourney[e.i][e.j] = 'W';
                        tourney[e.j][e.i] = 'L';
                        break;
                    case 2:
                        tourney[e.i][e.j] = 'w';
                        tourney[e.j][e.i] = 'l';
                        break;
                    case 1:
                        tourney[e.i][e.j] = 'l';
                        tourney[e.j][e.i] = 'w';
                        break;
                    case 0:
                        tourney[e.i][e.j] = 'L';
                        tourney[e.j][e.i] = 'W';
                        break;
                }
            }
        }
    }

    dump(tourney);
}