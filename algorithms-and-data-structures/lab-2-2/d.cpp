#include <iostream>

using namespace std;

typedef long long ll;

struct Node {
    ll key;
    int y;
    ll sum;
    Node *left, *right;

    Node(const ll &key) : key(key), left(nullptr), right(nullptr) {
        y = rand();
        sum = key;
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
    printf("%i %i\n", root->key, root->sum);
    dump(root->left, depth + 1);
    dump(root->right, depth + 1);
    if (depth == 0) {
        printf("-----------------\n");
    }
}

ll nodeSum(Node *&t) {
    return t != nullptr ? t->sum : 0;
}

void nodeUpdateSum(Node *&t) {
    if (t != nullptr) {
        t->sum = t->key + nodeSum(t->left) + nodeSum(t->right);
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
        nodeUpdateSum(root);
        res = {root, t.second};
    } else {
        Splitted t = split(root->left, key);
        root->left = t.second;
        nodeUpdateSum(root);
        res = {t.first, root};
    }
    return res;
}

Node *merge(Node *t1, Node *t2) {
    if (t1 == nullptr) {
        return t2;
    } else if (t2 == nullptr) {
        return t1;
    } else if (t1->y > t2->y) {
        t1->right = merge(t1->right, t2);
        nodeUpdateSum(t1);
        return t1;
    } else {
        t2->left = merge(t1, t2->left);
        nodeUpdateSum(t2);
        return t2;
    }
}

void nodeDelete(Node *&t, const ll &key) {
    if (t == nullptr) {
        return;
    }

    if (t->key == key) {
        t = merge(t->left, t->right);
        nodeUpdateSum(t);
    } else {
        nodeDelete(key < t->key ? t->left : t->right, key);
    }
}

Node *nodeSearch(Node *root, const ll &key) {
    if (root == nullptr || root->key == key) {
        return root;
    } else if (key < root->key) {
        return nodeSearch(root->left, key);
    } else { // root->key < key
        return nodeSearch(root->right, key);
    }
}

void insert(Node *&root, Node *it) {
    if (nodeSearch(root, it->key) != nullptr) {
        return;
    }

    Splitted t = split(root, it->key);
    root = merge(t.first, merge(it, t.second));
}

void nodeInsert(Node *&root, const ll &key) {
    insert(root, new Node(key));
}

ll sum(Node *&root, const ll &l, const ll &r) {
    if (root == nullptr) {
        return 0;
    }
    Splitted t1 = split(root, l);
    Splitted t2 = split(t1.second, r);

    ll res = nodeSum(t2.first);
//    printf("print split\n");
//    dump(t2.first);

    root = merge(t1.first, merge(t2.first, t2.second));

    return res;
}


int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    Node *root = nullptr;
    int n;
    cin >> n;
    ll last = 0;
    for (int i = 0; i < n; i++) {
        char op;
        cin >> op;
        if (op == '?') {
            int l, r;
            cin >> l >> r;
            last = sum(root, l, r + 1);
            cout << last << endl;
        } else if (op == '+') {
            ll x;
            cin >> x;
            nodeInsert(root, (x + last) % 1000000000);
            last = 0;
        }
    }
    return 0;
}