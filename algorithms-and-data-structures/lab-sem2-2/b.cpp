#include <iostream>

using namespace std;

typedef long long ll;

struct Node {
    ll key;
    int y;
    int count = 1;
    Node *left, *right;

    Node(const ll &key) : key(key), left(nullptr), right(nullptr) {
        y = rand();
    }
};

int nodeCount(Node *&t) {
    return t != nullptr ? t->count : 0;
}

void nodeUpdateCount(Node*& t) {
    if (t != nullptr) {
        t->count = 1 + nodeCount(t->left) + nodeCount(t->right);
    }
}

typedef pair<Node *, Node *> Splitted;

Splitted split(Node *&root, const ll &key) {
    if (root == nullptr) {
        return {nullptr, nullptr};
    }
    
    if (key > root->key) {
        Splitted t = split(root->right, key);
        root->right = t.first;
        return {root, t.second};
    } else {
        Splitted t = split(root->left, key);
        root->left = t.second;
        return {t.first, root};
    }
}

Node *merge(Node *t1, Node *t2) {
    if (t1 == nullptr) {
        return t2;
    } else if (t2 == nullptr) {
        return t1;
    } else if (t1->y > t2->y) {
        t1->right = merge(t1->right, t2);
        return t1;
    } else {
        t2->left = merge(t1, t2->left);
        return t2;
    }
}

Node *nodeMin(Node *root) {
    if (root->left == nullptr) {
        return root;
    }
    return nodeMin(root->left);
}

Node *nodeMax(Node *root) {
    if (root->right == nullptr) {
        return root;
    }
    return nodeMax(root->right);
}

void insert(Node *&root, Node *it) {
    if (root == nullptr) {
        root = it;
    } else if (it->y > root->y) {
        Splitted t = split(root, it->key);
        it->left = t.first;
        it->right = t.second;
        root = it;
    } else {
        insert(it->key < root->key ? root->left : root->right, it);
    }
}

void nodeDelete(Node *&t, const ll &key) {
    if (t == nullptr) {
        return;
    }

    if (t->key == key) {
        t = merge(t->left, t->right);
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

Node *nodeNext(Node *root, const ll &key) {
    Node *current = root, *successor = nullptr;
    while (current != nullptr) {
        if (current->key <= key) {
            current = current->right;
        } else {
            successor = current;
            current = current->left;
        }
    }
    return successor;
}

Node *nodePrev(Node *root, const ll &key) {
    Node *current = root, *successor = nullptr;
    while (current != nullptr) {
        if (current->key >= key) {
            current = current->left;
        } else {
            successor = current;
            current = current->right;
        }
    }
    return successor;
}

void nodeInsert(Node *&root, const ll &key) {
    if (nodeSearch(root, key) == nullptr) {
        insert(root, new Node(key));
    }
}


int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    Node *root = nullptr;
    string s;
    ll x;
    /*
     * insert key — добавить в дерево ключ key. Если ключ key есть в дереве, то ничего делать не надо;
     * delete key — удалить из дерева ключ key. Если ключа key в дереве нет, то ничего делать не надо;
     * exists key — если ключ key есть в дереве выведите «true», если нет «false»;
     * next key — выведите минимальный элемент в дереве, строго больший key, или «none» если такого нет;
     * prev key — выведите максимальный элемент в дереве, строго меньший key, или «none» если такого нет.
     */
    while (cin >> s) {
        cin >> x;
        if (s == "insert") {
            nodeInsert(root, x);
        } else if (s == "delete") {
            nodeDelete(root, x);
        } else if (s == "exists") {
            cout << (nodeSearch(root, x) == nullptr ? "false" : "true") << endl;
        } else if (s == "next") {
            Node *res = nodeNext(root, x);
            if (res == nullptr) {
                cout << "none" << endl;
            } else {
                cout << res->key << endl;
            }
        } else if (s == "prev") {
            Node *res = nodePrev(root, x);
            if (res == nullptr) {
                cout << "none" << endl;
            } else {
                cout << res->key << endl;
            }
        }
    }
    return 0;
}