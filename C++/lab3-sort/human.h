#ifndef LABORATORNAYA_RABOTA_3_SORTIROVKA_PECHHENKA_HUMAN_H
#define LABORATORNAYA_RABOTA_3_SORTIROVKA_PECHHENKA_HUMAN_H

#include <stddef.h>
#include <malloc.h>

typedef struct {
    char surname[21];
    char name[21];
    char patronymic[21];
    long long telephoneNumber;
} human_t;

int comparator(const human_t *a, const human_t *b);

#endif //LABORATORNAYA_RABOTA_3_SORTIROVKA_PECHHENKA_HUMAN_H
