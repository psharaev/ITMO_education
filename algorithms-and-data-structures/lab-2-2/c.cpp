#include <iostream>
#include <string>
#include <random>

using namespace std;

typedef long long ll;

struct Node {
    ll value;
    int y;
    int size = 1;
    bool zeroExists;
    Node *left, *right;

    explicit Node(const ll &key) : value(key), left(nullptr), right(nullptr) {
        static std::random_device rd;
        static std::mt19937 gen(rd());
        static std::uniform_int_distribution<int> distrib(INT_MIN, INT_MAX);
        y = (distrib(gen) << 15) + distrib(gen);
        zeroExists = key == 0;
    }
};

void dump(Node *&root, const int &depth = 0) {
    if (depth == 0) {
        printf("*****************\n");
    }

    for (int i = 0; i < depth; ++i) {
        printf(" ");
    }

    if (root == nullptr) {
        printf("none\n");
        return;
    }
    printf("%i %i\n", root->value, root->size);
    dump(root->left, depth + 1);
    dump(root->right, depth + 1);
    if (depth == 0) {
        printf("-----------------\n");
    }
}

void dump2(Node *root) {
    if (root->left != nullptr) {
        dump2(root->left);
    }
    cout << root->value << ' ';
    if (root->right != nullptr) {
        dump2(root->right);
    }
}

int nodeCount(Node *t) {
    return t != nullptr ? t->size : 0;
}

void dump2Shell(Node *root) {
//    cout << "***" << endl;
//    cout << nodeCount(root) << endl;
    if (root != nullptr) {
        dump2(root);
    }
//    cout << endl << "---" << endl;
}

bool nodeZero(Node *root) {
    return root != nullptr && root->zeroExists;
}

void nodeUpdate(Node *t) {
    if (t != nullptr) {
        t->size = 1 + nodeCount(t->left) + nodeCount(t->right);
        t->zeroExists = (t->value == 0) || nodeZero(t->left) || nodeZero(t->right);
    }
}

typedef pair<Node *, Node *> Splitted;

Splitted split(Node *&root, const ll &key, const int &add = 0) {
    if (root == nullptr) {
        return {nullptr, nullptr};
    }

    Splitted res;

    int cur_key = add + nodeCount(root->left);

    if (key > cur_key) {
        Splitted t = split(root->right, key, 1 + cur_key);
        root->right = t.first;
        res = {root, t.second};
    } else {
        Splitted t = split(root->left, key, add);
        root->left = t.second;
        res = {t.first, root};
    }
    nodeUpdate(root);
    return res;
}

Node *merge(Node *t1, Node *t2) {
    Node *res;
    if (t1 == nullptr) {
        res = t2;
    } else if (t2 == nullptr) {
        res = t1;
    } else if (t1->y > t2->y) {
        t1->right = merge(t1->right, t2);
        res = t1;
    } else {
        t2->left = merge(t1, t2->left);
        res = t2;
    }
    nodeUpdate(res);
    return res;
}

void splitByZero(Node *root, Node *&left_node, Node *&right_node, Node *root_node, int key = 0) {
    if (!nodeZero(root)) {
        Splitted res = split(root_node, nodeCount(root) + key);
        left_node = res.first;
        right_node = res.second;
        return;
    } else if (nodeZero(root->left) || root->value == 0) {
        splitByZero(root->left, left_node, right_node, root_node, key);
    } else {
        splitByZero(root->right, left_node, right_node, root_node, key + nodeCount(root->left) + 1);
    }
}

void nodeStartInsert(Node *&root, Node *it, const int &index) {
    Splitted t = split(root, index);
    root = merge(t.first, merge(it, t.second));
    nodeUpdate(root);
}

void nodeStartInsertShell(Node *&root, const ll &value, const int &index) {
    nodeStartInsert(root, new Node(value), index);
}

void nodeWorkInsert(Node *&root, int index, int value) {
    Node *mid_node = nullptr;
    Splitted t1 = split(root, index);
    splitByZero(t1.second, mid_node, t1.second, t1.second);
    Splitted t2 = split(t1.second, 1);

    root = merge(t1.first, new Node(value));
    root = merge(root, mid_node);
    root = merge(root, t2.second);
}

void toArray(Node *root, vector<ll> &arr) {
    if (root == nullptr) {
        return;
    }
    toArray(root->left, arr);
    arr.push_back(root->value);
    toArray(root->right, arr);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    Node *root = nullptr;

    int n, m;
    cin >> n >> m;
    for (int i = 0; i < m; i++) {
        nodeStartInsertShell(root, 0, i);
    }

    for (int i = 0; i < n; i++) {
        int index;
        cin >> index;
        nodeWorkInsert(root, index - 1, i + 1);
    }

    vector<ll> ans = vector<ll>();
    toArray(root, ans);

    while (!ans.empty() && ans.back() == 0) {
        ans.pop_back();
    }

    cout << ans.size() << endl;
    for (ll item : ans) {
        cout << item << ' ';
    }

    return 0;
}