#ifndef LAB1_LINEARSYSTEM_H
#define LAB1_LINEARSYSTEM_H

#include <stdlib.h>
#include <memory.h>

struct LinearSystem_t {
    int n;
    int *order;
    float **a;
    float *b;
};

struct LinearSystem_t *constructor(int n);

struct LinearSystem_t *copy(const struct LinearSystem_t *system);

void freeLinearSystem(struct LinearSystem_t *system);

#endif //LAB1_LINEARSYSTEM_H
