#include <iostream>

using namespace std;

typedef long long ll;

struct Node {
    ll key;
    Node *l = nullptr, *r = nullptr;//, *p = nullptr;

    Node(const ll &key) : key(key) {}
};

Node *nodeMin(Node *root) {
    if (root->l == nullptr) {
        return root;
    }
    return nodeMin(root->l);
}

Node *nodeMax(Node *root) {
    if (root->r == nullptr) {
        return root;
    }
    return nodeMax(root->r);
}

void nodeInsert(Node *&root, const ll &key) {
    if (root == nullptr) {
        root = new Node(key);
    } else if (key < root->key) {
        nodeInsert(root->l, key);
    } else if (root->key < key) {
        nodeInsert(root->r, key);
    }
}

Node *nodeDelete(Node *&root, const ll &key) {
    if (root == nullptr) {
        return root;
    }
    if (key < root->key) {
        root->l = nodeDelete(root->l, key);
    } else if (key > root->key) {
        root->r = nodeDelete(root->r, key);
    } else if (root->l != nullptr && root->r != nullptr) {
        root->key = nodeMin(root->r)->key;
        root->r = nodeDelete(root->r, root->key);
    } else {
        if (root->l != nullptr) {
            root = root->l;
        } else if (root->r != nullptr) {
            root = root->r;
        } else {
            root = nullptr;
        }
    }
    return root;
}

Node *nodeSearch(Node *root, const ll &key) {
    if (root == nullptr || root->key == key) {
        return root;
    } else if (key < root->key) {
        return nodeSearch(root->l, key);
    } else { // root->key < key
        return nodeSearch(root->r, key);
    }
}

Node *nodeNext(Node *root, const ll &key) {
    Node *current = root, *successor = nullptr;
    while (current != nullptr) {
        if (current->key <= key) {
            current = current->r;
        } else {
            successor = current;
            current = current->l;
        }
    }
    return successor;
}

Node *nodePrev(Node *root, const ll &key) {
    Node *current = root, *successor = nullptr;
    while (current != nullptr) {
        if (current->key >= key) {
            current = current->l;
        } else {
            successor = current;
            current = current->r;
        }
    }
    return successor;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    Node *root = nullptr;
    string s;
    ll x;
    /*
     * insert x — добавить в дерево ключ x. Если ключ x есть в дереве, то ничего делать не надо;
     * delete x — удалить из дерева ключ x. Если ключа x в дереве нет, то ничего делать не надо;
     * exists x — если ключ x есть в дереве выведите «true», если нет «false»;
     * next x — выведите минимальный элемент в дереве, строго больший x, или «none» если такого нет;
     * prev x — выведите максимальный элемент в дереве, строго меньший x, или «none» если такого нет.
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