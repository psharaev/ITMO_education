#ifndef LAB1_LINEARSYSTEMSOLVE_H
#define LAB1_LINEARSYSTEMSOLVE_H

#include "LinearSystem.h"

#include <stdlib.h>
#include <math.h>

#define LINEAR_SYSTEM_CERTAIN 0
#define LINEAR_SYSTEM_MANY_SOLUTION 1
#define LINEAR_SYSTEM_NO_SOLUTION 2

struct LinearSystemSolveResult_t {
    int codeResult;

    int countRoots;
    float *rots;
};

void freeLinearSystemSolveResult(struct LinearSystemSolveResult_t *s);

struct LinearSystemSolveResult_t *solve(const struct LinearSystem_t *system, int *errorCode);

#endif //LAB1_LINEARSYSTEMSOLVE_H
