#include <memory.h>

#include "vector.h"

static void ensureCapacity(vector_t *v, size_t capacity);

static VECTOR_TYPE *arr;

vector_t createVector(size_t capacity)
{
    vector_t v;
    v.errorCode = 0;

    v.arr = malloc(sizeof(VECTOR_TYPE) * capacity);
    if (v.arr == NULL) {
        v.errorCode = 2;
    }
    v.size = 0;
    v.capacity = capacity;
    return v;
}

void free_vector_t(vector_t v)
{
    free(v.arr);
}

void pushBack(vector_t *v, const VECTOR_TYPE *item)
{
    ensureCapacity(v, (int) v->size + 1);
    if (v->errorCode != 0) {
        return;
    }
    v->arr[v->size++] = *item;
}

static void quickSortImpl(int left, int right)
{
    while (left < right) {
        int i = left, j = right;
        VECTOR_TYPE pivot = arr[(left + right) / 2];

        while (i <= j) {
            while (comparator(arr + i, &pivot) < 0) {
                i++;
            }
            while (comparator(arr + j, &pivot) > 0) {
                j--;
            }

            if (i <= j) {
                VECTOR_TYPE temp = arr[i];
                arr[i++] = arr[j];
                arr[j--] = temp;
            }
        }

        if (j - left < right - i) {
            quickSortImpl(left, j);
            left = i;
        } else {
            quickSortImpl(i, right);
            right = j;
        }
    }
}

void quickSort(vector_t *v)
{
    if (v->size <= 1) {
        return;
    }

    arr = v->arr;
    quickSortImpl(0, (int) v->size - 1);
}

static void ensureCapacity(vector_t *v, const size_t capacity)
{
    if (capacity <= v->capacity) {
        return;
    }

    size_t newCapacity = capacity * 2;
    VECTOR_TYPE *newArr = realloc(sizeof(VECTOR_TYPE) * newCapacity);
    if (newArr == NULL) {
        v->errorCode = 2;
        return;
    }

//    if (v->arr != newArr) { 
//        free(v->arr);
//    }
    v->arr = newArr;
    v->capacity = newCapacity;
}