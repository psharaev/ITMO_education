#include <iostream>
#include <stdlib.h>
#include <random>

using namespace std;

typedef long long ll;

struct Node {
    ll key;
    int y;
    int count = 1;
    Node *left, *right;

    explicit Node(const ll &key) : key(key), left(nullptr), right(nullptr) {
        static std::random_device rd;
        static std::mt19937 gen(rd());
        static std::uniform_int_distribution<int> distrib(INT_MIN, INT_MAX);
        y = (distrib(gen) << 15) + distrib(gen);
    }
};

void dump(Node *&root, const int &depth = 0) {
    if (depth == 0) {
        printf("*****************\n");
    }
    if (root == nullptr) {
        return;
    }

    for (int i = 0; i < depth; ++i) {
        printf(" ");
    }
    printf("%i %i\n", root->key, root->count);
    dump(root->left, depth + 1);
    dump(root->right, depth + 1);
    if (depth == 0) {
        printf("-----------------\n");
    }
}

int nodeCount(Node *t) {
    return t != nullptr ? t->count : 0;
}

void nodeUpdate(Node *t) {
    if (t != nullptr) {
        t->count = 1 + nodeCount(t->left) + nodeCount(t->right);
    }
}

typedef pair<Node *, Node *> Splitted;

Splitted split(Node *&root, const ll &key) {
    Splitted res;
    if (root == nullptr) {
        res = {nullptr, nullptr};
    } else if (key > root->key) {
        Splitted t = split(root->right, key);
        root->right = t.first;
        res = {root, t.second};
    } else {
        Splitted t = split(root->left, key);
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

void nodeDelete(Node *&t, const ll &key) {
    Splitted t1 = split(t, key);
    Splitted t2 = split(t1.second, key + 1);
    t = merge (t1.first, t2.second);
}

Node *nodeSearch(Node *root, const ll &key) {
    if (root == nullptr || root->key == key) {
        return root;
    } else if (key < root->key) {
        return nodeSearch(root->left, key);
    } else {
        return nodeSearch(root->right, key);
    }
}

void insert(Node *&root, Node *it) {
    if (nodeSearch(root, it->key) != nullptr) {
        return;
    }

    Splitted t = split(root, it->key);
    root = merge(t.first, merge(it, t.second));
    nodeUpdate(root);
}

void nodeInsert(Node *&root, const ll &key) {
    insert(root, new Node(key));
}

ll nodeGet(Node *&root, const ll &k) {
    ll cnt = nodeCount(root->left);
    if (cnt == k) {
        return root->key;
    } else if (k < cnt) {
        return nodeGet(root->left, k);
    } else {
        return nodeGet(root->right, k - cnt - 1);
    }
}


int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);
    Node *root = nullptr;
    int n;
    cin >> n;
    ll treeSize = 0;
    for (int i = 0; i < n; i++) {
        ll op, x;
        cin >> op >> x;
//        cout << op << x << "kek" << endl;
        if (op == 1) {
            nodeInsert(root, x);
            treeSize++;
        } else if (op == 0) {
            cout << nodeGet(root, treeSize - x) << endl;
        } else if (op == -1) {
            nodeDelete(root, x);
            treeSize--;
        }
    }
    return 0;
}