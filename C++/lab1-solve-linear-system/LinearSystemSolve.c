#include "LinearSystemSolve.h"

void freeLinearSystemSolveResult(struct LinearSystemSolveResult_t *s)
{
    if (s != NULL) {
        free(s->rots);
        free(s);
    }
}

void findMaxByAbsInMatrix(const struct LinearSystem_t *s, int startLine, int startColumn, int *line, int *column)
{
    *line = startLine;
    *column = startColumn;
    float best = fabsf(s->a[startLine][startColumn]);
    for (int iLine = startLine; iLine < s->n; ++iLine) {
        for (int jColumn = startColumn; jColumn < s->n; ++jColumn) {
            if (fabsf(s->a[iLine][jColumn]) > best) {
                best = fabsf(s->a[iLine][jColumn]);
                *line = iLine;
                *column = jColumn;
            }
        }
    }
}

void swapLines(struct LinearSystem_t *s, const int a, const int b)
{
    if (a == b) {
        return;
    }

    float *temp = s->a[a];
    s->a[b] = s->a[a];
    s->a[a] = temp;
}

void swapRoots(struct LinearSystem_t *s, const int a, const int b)
{
    if (a == b) {
        return;
    }

    int tempOrder = s->order[a];
    s->order[a] = s->order[b];
    s->order[b] = tempOrder;

    for (int i = 0; i < s->n; ++i) {
        float tempRoot = s->a[i][a];
        s->a[i][a] = s->a[i][b];
        s->a[i][b] = tempRoot;
    }
}

void destroyNextLinesThisLine(struct LinearSystem_t *s, const int idLine, const int startRoot)
{
    for (int line = idLine + 1; line < s->n; ++line) {
        if (s->a[line][startRoot] == 0.f) {
            continue;
        }

        float k = s->a[line][startRoot] / s->a[idLine][startRoot];
        s->a[line][startRoot] = 0.f;
        for (int column = startRoot + 1; column < s->n; ++column) {
            s->a[line][column] -= k * s->a[idLine][column];
        }
        s->b[line] -= k * s->b[idLine];
    }
}

float *collectAnswer(const struct LinearSystem_t *s)
{
    float *res = (float *) malloc(s->n * sizeof(float));
    if (res == NULL) {
        return NULL;
    }

    for (int line = s->n - 1; line >= 0; --line) {
        float sum = 0.f;
        for (int column = line + 1; column < s->n; ++column) {
            sum += res[s->order[column]] * s->a[line][column];
        }
        res[s->order[line]] = (s->b[line] - sum) / s->a[line][line];
    }
    return res;
}

struct LinearSystemSolveResult_t *solve(const struct LinearSystem_t *system, int *errorCode)
{
    struct LinearSystem_t *s = copy(system);
    if (s == NULL) {
        *errorCode = 2;
        return NULL;
    }

    struct LinearSystemSolveResult_t *res = (struct LinearSystemSolveResult_t *) malloc(
            sizeof(struct LinearSystemSolveResult_t));
    if (res == NULL) {
        *errorCode = 2;
        freeLinearSystem(s);
        return NULL;
    }

    res->codeResult = -1;
    res->countRoots = 0;
    res->rots = NULL;

    int n = s->n;
    int *findLine = malloc(sizeof(int)), *findColumn = malloc(sizeof(int));
    if (findLine == NULL || findColumn == NULL) {
        *errorCode = 2;
        free(findLine);
        free(findColumn);
        return NULL;
    }

    for (int line = 0; line < n; ++line) {
        if (s->a[line][line] == 0.f) {
            findMaxByAbsInMatrix(s, line, line, findLine, findColumn);
            if (s->a[*findLine][*findColumn] == 0.f) {
                if (s->b[*findLine] == 0.f) {
                    res->codeResult = LINEAR_SYSTEM_MANY_SOLUTION;
                } else {
                    res->codeResult = LINEAR_SYSTEM_NO_SOLUTION;
                }
                return res;
            }
            swapRoots(s, line, *findColumn);
            swapLines(s, line, *findLine);
        }
        destroyNextLinesThisLine(s, line, line);
    }

    free(findLine);
    free(findColumn);

    res->codeResult = LINEAR_SYSTEM_CERTAIN;
    res->countRoots = s->n;
    res->rots = collectAnswer(s);
    freeLinearSystem(s);
    if (res->rots == NULL) {
        *errorCode = 2;
        freeLinearSystemSolveResult(res);
        return NULL;
    }

    *errorCode = 0;
    return res;
}