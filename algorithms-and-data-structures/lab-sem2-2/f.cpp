#include <iostream>
#include <string>
#include <random>

using namespace std;

typedef long long ll;

struct Node {
    ll value;
    int y;
    int size = 1;
    Node *left, *right;

    explicit Node(const ll &key) : value(key), left(nullptr), right(nullptr) {
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
    cout << nodeCount(root) << endl;
    if (root != nullptr) {
        dump2(root);
    }
//    cout << endl << "---" << endl;
}

void nodeUpdate(Node *t) {
    if (t != nullptr) {
        t->size = 1 + nodeCount(t->left) + nodeCount(t->right);
    }
}

typedef pair<Node *, Node *> Splitted;

Splitted split(Node *&root, const ll &key, const int &add = 0) {
    Splitted res;

    int cur_key;
    if (root != nullptr) {
        cur_key = add + nodeCount(root->left);
    }

    if (root == nullptr) {
        res = {nullptr, nullptr};
    } else if (key > cur_key) {
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

void nodeDelete(Node *&root, const ll &key) {
//    cout << "in del" << endl;
//    dump(root);
    Splitted t1 = split(root, key - 1);
//    dump(t1.first);
//    dump(t1.second);
    Splitted t2 = split(t1.second, key - nodeCount(t1.first));
//    dump(t2.first);
//    dump(t2.second);
    root = merge(t1.first, t2.second);
}

Node *nodeSearch(Node *root, const ll &key) {
    if (root == nullptr || root->value == key) {
        return root;
    } else if (key < root->value) {
        return nodeSearch(root->left, key);
    } else {
        return nodeSearch(root->right, key);
    }
}

void insert(Node *&root, Node *it, const int &index) {
//    if (nodeSearch(root, it->value) != nullptr) {
//        return;
//    }

    Splitted t = split(root, index);
    root = merge(t.first, merge(it, t.second));
    nodeUpdate(root);
}

void nodeInsert(Node *&root, const ll &value, const int &index) {
    insert(root, new Node(value), index);
}

ll nodeGet(Node *&root, const ll &k) {
    ll cnt = nodeCount(root->left);
    if (cnt == k) {
        return root->value;
    } else if (k < cnt) {
        return nodeGet(root->left, k);
    } else {
        return nodeGet(root->right, k - cnt - 1);
    }
}

void rotate(Node *&root, const ll &l, const ll &r) {
    Splitted t1 = split(root, l);
    Splitted t2 = split(t1.second, r - l + 1);
    root = merge(t2.first, merge(t1.first, t2.second));
}

void add(Node *&root, const ll &index, Node *v) {
    Splitted t = split(root, index);
    root = merge(t.first, merge(v, t.second));
}

int main() {
//    ios::sync_with_stdio(false);
//    cin.tie(nullptr);
//    cout.tie(nullptr);

    Node *root = nullptr;

    int n, m;
    cin >> n >> m;
    for (int i = 1; i <= n; ++i) {
        int item;
        cin >> item;
        nodeInsert(root, item, i);
    }

//    dump(root);
    for (int i = 0; i < m; i++) {
        string op;
        cin >> op;
        if (op == "del") {
            ll index;
            cin >> index;
            nodeDelete(root, index);
//            cout << "complete del " << index << endl;
//            dump2Shell(root);
        } else if (op == "add") {
            ll index, value;
            cin >> index >> value;
            add(root, index, new Node(value));
//            cout << "complete add " << index << ' ' << value << endl;
//            dump2Shell(root);
        }
    }

    dump2Shell(root);

    return 0;
}