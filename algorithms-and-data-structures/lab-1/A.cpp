#include <iostream>
#include <vector>

using namespace std;

vector<int> subVector(const vector<int> &v, int start, int end); // [start, end)
vector<int> merge(const vector<int> &a, const vector<int> &b);

vector<int> insertionSort(const vector<int> &v);

vector<int> insertionSort(const vector<int> &v) {
    int n = v.size();
    if (n <= 1) return v;

    vector<int> a = subVector(v, 0, n / 2);
    vector<int> b = subVector(v, n / 2, n);
    a = insertionSort(a);
    b = insertionSort(b);

    return merge(a, b);
}

vector<int> subVector(const vector<int> &v, int start, int end) {
    int n = end - start;
    vector<int> res(n);
    for (int i = start; i < end; ++i) {
        res[i - start] = v[i];
    }
    return res;
}

vector<int> merge(const vector<int> &a, const vector<int> &b) {
    vector<int> c(a.size() + b.size());
    int n = a.size();
    int m = b.size();
    int i = 0, j = 0, k = 0;

    while (i < n || j < m) {
        if (j == m || (i < n && a[i] < b[j]))
            c[k++] = a[i++];
        else
            c[k++] = b[j++];
    }

    return c;
}

int main() {
    int n;
    cin >> n;

    vector<int> arr(n);
    for (int i = 0; i < n; ++i) {
        cin >> arr[i];
    }

    arr = insertionSort(arr);

    for (int i = 0; i < n; ++i) {
        cout << arr[i] << ' ';
    }
}
