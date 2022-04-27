#include <stdio.h>

#include "LinearSystem.h"
#include "LinearSystemSolve.h"

struct LinearSystem_t *readLinearSystem(const char *fileName, int *errorCode)
{
    FILE *inFile = fopen(fileName, "r");
    if (inFile == NULL) {
        *errorCode = 1;
        return NULL;
    }

    int n;
    fscanf(inFile, "%i", &n);
    struct LinearSystem_t *res = constructor(n);
    if (res == NULL) {
        *errorCode = 2;
        return NULL;
    }

    for (int i = 0; i < n; ++i) {
        res->order[i] = i;
        for (int j = 0; j < n; ++j) {
            fscanf(inFile, "%f", &(res->a[i][j]));
        }
        fscanf(inFile, "%f", &(res->b[i]));
    }

    fclose(inFile);
    *errorCode = 0;
    return res;
}

void writeResult(const char *fileName, const struct LinearSystemSolveResult_t *result, int *errorCode)
{
    FILE *outFile = fopen(fileName, "w");

    if (outFile == NULL) {
        *errorCode = 1;
        return;
    }

    if (result->codeResult == LINEAR_SYSTEM_CERTAIN) {
        for (int i = 0; i < result->countRoots; ++i) {
            fprintf(outFile, "%g\n", result->rots[i]);
        }
    } else if (result->codeResult == LINEAR_SYSTEM_NO_SOLUTION) {
        fprintf(outFile, "no solution\n");
    } else if (result->codeResult == LINEAR_SYSTEM_MANY_SOLUTION) {
        fprintf(outFile, "many solution\n");
    }

    fclose(outFile);
    *errorCode = 0;
}

int main(int argc, char *argv[])
{
    if (argc != 3) {
        printf("enter 2 arguments:\n - input file name\n - output file name");
        return 1;
    }

    int *errorCode = malloc(sizeof(int));
    if (errorCode == NULL) {
        printf("failed create error code\n");
        return 2;
    }

    struct LinearSystem_t *system = readLinearSystem(argv[1], errorCode);
    if (*errorCode == 1) {
        printf("failed read input file: %s\n", argv[1]);
        free(errorCode);
        freeLinearSystem(system);
        return 1;
    } else if (*errorCode == 2) {
        printf("failed allocate memory\n");
        free(errorCode);
        freeLinearSystem(system);
        return 2;
    }

    struct LinearSystemSolveResult_t *res = solve(system, errorCode);
    freeLinearSystem(system);
    if (*errorCode == 2) {
        printf("failed allocate memory to solve system\n");
        free(errorCode);
        freeLinearSystemSolveResult(res);
        return 2;
    }

    writeResult(argv[2], res, errorCode);
    freeLinearSystemSolveResult(res);
    if (*errorCode == 1) {
        printf("failed write answer to output file: %s", argv[2]);
        free(errorCode);
        return 1;
    }

    free(errorCode);
    return 0;
}
