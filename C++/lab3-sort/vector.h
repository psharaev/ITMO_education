#ifndef LABORATORNAYA_RABOTA_3_SORTIROVKA_PECHHENKA_VECTOR_H
#define LABORATORNAYA_RABOTA_3_SORTIROVKA_PECHHENKA_VECTOR_H

#include <stddef.h>
#include "human.h"

typedef human_t VECTOR_TYPE;

typedef struct {
    VECTOR_TYPE *arr;
    size_t size;
    size_t capacity;
    int errorCode;
} vector_t;

vector_t createVector(size_t capacity);

void free_vector_t(vector_t v);

void pushBack(vector_t *v, const VECTOR_TYPE *item);

void quickSort(vector_t *v);

#endif //LABORATORNAYA_RABOTA_3_SORTIROVKA_PECHHENKA_VECTOR_H
