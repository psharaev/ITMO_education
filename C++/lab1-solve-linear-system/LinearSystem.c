#include "LinearSystem.h"

struct LinearSystem_t *constructor(const int n)
{
    struct LinearSystem_t *res = malloc(sizeof(struct LinearSystem_t));
    if (res == NULL) {
        return NULL;
    }

    res->n = n;

    res->order = malloc(n * sizeof(int));
    if (res->order == NULL) {
        return NULL;
    }

    res->a = malloc(n * sizeof(float *));
    if (res->a == NULL) {
        return NULL;
    }

    for (int i = 0; i < n; ++i) {
        res->a[i] = malloc(n * sizeof(float));
        if (res->a[i] == NULL) {
            return NULL;
        }
    }

    res->b = malloc(n * sizeof(float));
    if (res->b == NULL) {
        return NULL;
    }

    return res;
}

struct LinearSystem_t *copy(const struct LinearSystem_t *system)
{
    if (system == NULL) {
        return NULL;
    }

    int n = system->n;

    struct LinearSystem_t *res = constructor(n);
    if (res == NULL) {
        return NULL;
    }

    memcpy(res->order, system->order, n * sizeof(int));
    memcpy(res->b, system->b, n * sizeof(float *));
    for (int i = 0; i < n; ++i) {
        memcpy(res->a[i], system->a[i], n * sizeof(float));
    }

    return res;
}

void freeLinearSystem(struct LinearSystem_t *system)
{
    if (system != NULL) {
        free(system->order);
        if (system->a != NULL) {
            for (int i = 0; i < system->n; ++i) {
                free(system->a[i]);
            }
        }
        free(system->a);
        free(system->b);
        free(system);
    }
}