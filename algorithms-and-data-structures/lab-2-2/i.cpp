#include <iostream>
#include <string>
#include <climits>
#include <random>

using namespace std;

struct Node {
    int key, count = 1, y;
    bool type;
    Node *parent = nullptr, *left = nullptr, *right = nullptr;

    explicit Node(int value, bool type = false) : key(value), type(type) {
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
    printf("%i %i\n", root->key, root->count);
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
    cout << root->key << ' ';
    if (root->right != nullptr) {
        dump2(root->right);
    }
}

int nodeCount(Node *t) {
    return t != nullptr ? t->count : 0;
}

void dump2Shell(Node *root) {
//    cout << "***" << endl;
//    cout << nodeCount(root) << endl;
    if (root != nullptr) {
        dump2(root);
    }
//    cout << endl << "---" << endl;
}

void nodeUpdate(Node *root) {
    if (root == nullptr) {
        return;
    }
    root->count = 1 + nodeCount(root->left) + nodeCount(root->right);
}

void setParent(Node *child, Node *parent) {
    if (child != nullptr) {
        child->parent = parent;
    }
}

void split(Node *root, Node *&resL, Node *&resR, int value) {
    if (root == nullptr) {
        resL = resR = nullptr;
        return;
    }
    int rootSize = nodeCount(root->left);
    if (value <= rootSize) {
        split(root->left, resL, root->left, value);
        resR = root;
        setParent(root->left, resR);
        setParent(root->right, resR);
        setParent(resL, nullptr);
    } else {
        split(root->right, root->right, resR, value - rootSize - 1);
        resL = root;
        setParent(root->left, resL);
        setParent(root->right, resL);
        setParent(resR, nullptr);
    }
    nodeUpdate(root);
}

Node *merge(Node *left_node, Node *right_node) {
    if (left_node == nullptr) {
        return right_node;
    }
    if (right_node == nullptr) {
        return left_node;
    }
    if (left_node->y > right_node->y) {
        left_node->right = merge(left_node->right, right_node);
        left_node->right->parent = left_node;
        nodeUpdate(left_node);
        return left_node;
    } else {
        right_node->left = merge(left_node, right_node->left);
        right_node->left->parent = right_node;
        nodeUpdate(right_node);
        return right_node;
    }
}

Node *nodeRoot(Node *&root) {
    Node *cur = root;
    while (cur->parent != nullptr) {
        cur = cur->parent;
    }
    return cur;
}

int nodeIndex(Node *&root) {
    bool from_right;
    Node *cur_root = root;
    int result = nodeCount(root->left);
    while (cur_root->parent != nullptr) {
        from_right = (cur_root->parent->right == cur_root);
        cur_root = cur_root->parent;
        result += from_right ? 1 + nodeCount(cur_root->left) : 0;
    }
    return result;
}

int nodeCity(Node *&root, const bool &type) {
    Node *cur_root = root;
    while ((type && cur_root->right != nullptr) || (!type && cur_root->left != nullptr)) {
        cur_root = type ? cur_root->right : cur_root->left;
    }
    return cur_root->key;
}

void nodeForceReverse(Node *&root) {
    if (root == nullptr) {
        return;
    }
    if (root->left != nullptr) {
        nodeForceReverse(root->left);
    }
    if (root->right != nullptr) {
        nodeForceReverse(root->right);
    }
    swap(root->left, root->right);
}

void setType(Node *&root, const bool &type) {
    if (root == nullptr) {
        return;
    }
    root->type = type;
    setType(root->left, type);
    setType(root->right, type);
}

void addRoad(vector<Node *> &cities, const int &a, const int &b) {
    Node *t1 = nodeRoot(cities[a]);
    Node *t2 = nodeRoot(cities[b]);
    if (t1 == t2) {
        setType(t1, true);
        return;
    } else {
        int cityL = nodeCity(t1, true);
        int cityR = nodeCity(t2, false);
        if (cityL != a && cityR != b) {
            swap(t1, t2);
        } else if (cityL == a && cityR != b) {
            nodeForceReverse(t2);
        } else if (cityL != a && cityR == b) {
            nodeForceReverse(t1);
        }
    }
    merge(t1, t2);
}

void shiftLeft(Node *&root, const int &pos) {
    Node *t1 = nullptr;
    Node *t2 = nullptr;
    split(root, t1, t2, pos);
    root = merge(t2, t1);
    nodeUpdate(root);
}

void removeRoad(vector<Node *> &cities, const int &a, const int &b) {
    Node *t1 = nodeRoot(cities[a]);
    Node *t2 = nodeRoot(cities[b]);
    int ind1 = nodeIndex(cities[a]);
    int ind2 = nodeIndex(cities[b]);
    if (t1 != t2) {
        return;
    }
    if (t1->type) {
        setType(t1, false);
        if ((ind1 == 0 && ind2 == nodeCount(t1) - 1) || (ind2 == 0 && ind1 == nodeCount(t1) - 1)) {
            return;
        }
        shiftLeft(t1, max(ind1, ind2));
        return;
    } else {
        if (ind1 < ind2) {
            split(t1, t1, t2, ind1 + 1);
        } else {
            split(t2, t1, t2, ind2 + 1);
        }
    }
}

int checkPath(vector<Node *> &cities, const int &a, const int &b) {
    Node *t1 = nodeRoot(cities[a]);
    Node *t2 = nodeRoot(cities[b]);
    int ind1 = nodeIndex(cities[a]);
    int ind2 = nodeIndex(cities[b]);

    if (t1 != t2) {
        return -1;
    } else {
        if (t1->type) {
            return min(abs(ind1 - ind2), nodeCount(t1) - abs(ind1 - ind2)) - 1;
        } else {
            return abs(ind1 - ind2) - 1;
        }
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int city1, city2;
    char op;
    int n, m, q;
    cin >> n >> m >> q;
    vector<Node *> cities(n);

    for (int i = 0; i < n; ++i) {
        cities[i] = new Node(i);
    }

    for (int i = 0; i < m; i++) {
        cin >> city1 >> city2;
        addRoad(cities, city1 - 1, city2 - 1);
    }

    for (int i = 0; i < q; i++) {
        cin >> op >> city1 >> city2;
        if (op == '+') {
            if (city1 != city2) {
                addRoad(cities, city1 - 1, city2 - 1);
            }
        } else if (op == '-') {
            if (city1 != city2) {
                removeRoad(cities, city1 - 1, city2 - 1);
            }
        } else if (op == '?') {
            if (city1 != city2) {
                cout << checkPath(cities, city1 - 1, city2 - 1) << endl;
            } else {
                cout << 0 << endl;
            }
        }
    }

    return 0;
}