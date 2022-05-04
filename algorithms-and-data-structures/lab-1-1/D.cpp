#include <iostream>
#include <vector>

using namespace std;

struct HeapMax {
    vector<int> v;
    int size;

};

void siftUp(HeapMax &hp, int i) { // Больший элемент поднимаем наверх
    while (hp.v[i] > hp.v[(i - 1) / 2]) {
        swap(hp.v[i], hp.v[(i - 1) / 2]);
        i = (i - 1) / 2;
    }
}

void siftDown(HeapMax &hp, int i) { // Меньший элемент опускаем вниз
    while (2 * i + 1 < hp.size) {
        int l = 2 * i + 1;
        int r = 2 * i + 2;
        int j = l;
        if (r < hp.size && hp.v[r] > hp.v[l])
            j = r;
        if (hp.v[i] >= hp.v[j])
            break;
        swap(hp.v[i], hp.v[j]);
        i = j;
    }
}

void insert(HeapMax &hp, const int &x) {
    hp.v[hp.size++] = x;
    siftUp(hp, hp.size - 1);
}

int extract(HeapMax &hp) {
    int max = hp.v[0];
    hp.v[0] = hp.v[--hp.size];
    siftDown(hp, 0);
    return max;
}

int main() {

    int n;
    cin >> n;

    HeapMax hp;
    hp.v = vector<int>(n + 1);
    hp.size = 0;

    for (int i = 0; i < n; ++i) {
        int f;
        cin >> f;

        if (f == 0) { // insert
            int x;
            cin >> x;
            insert(hp, x);
        } else if (f == 1) { // extract
            cout << extract(hp) << endl;
        }
    }

    return 0;
}
