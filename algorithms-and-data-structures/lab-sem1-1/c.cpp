#include <iostream>
#include <vector>

using namespace std;

long long int mergeSort(int *arr, int *temp, long long int left, long long int right);

long long int merge(int *arr, int *temp, long long int left, long long int mid, long long int right);

long long int mergeSort(int *arr, int *temp, long long int left, long long int right) {
    long long mid, count = 0;
    if (right > left) {
        mid = (right + left) / 2;

        count += mergeSort(arr, temp, left, mid);
        count += mergeSort(arr, temp, mid + 1, right);

        count += merge(arr, temp, left, mid + 1, right);
    }
    return count;
}

long long int merge(int *arr, int *temp, long long int left, long long int mid, long long int right) {
    long long i = left, j = mid, k = left;
    long long count = 0;

    while ((i + 1 <= mid) && (j <= right)) {
        if (arr[i] <= arr[j]) {
            temp[k++] = arr[i++];
        } else {
            temp[k++] = arr[j++];
            count += mid - i;
        }
    }

    while (i + 1 <= mid)
        temp[k++] = arr[i++];

    while (j <= right)
        temp[k++] = arr[j++];

    for (i = left; i <= right; ++i)
        arr[i] = temp[i];

    return count;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n;
    cin >> n;

    int *arr = new int[n];
    int *temp = new int[n];
    for (int i = 0; i < n; ++i) {
        cin >> arr[i];
        temp[i] = arr[i];
    }

    cout << mergeSort(arr, temp, 0, n - 1);
}
