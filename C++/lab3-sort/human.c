#include <string.h>

#include "human.h"

typedef int (*COMP_POINTER)(const human_t *, const human_t *);

static int compBySurname(const human_t *a, const human_t *b)
{
    return strcmp(a->surname, b->surname);
}

static int compByName(const human_t *a, const human_t *b)
{
    return strcmp(a->name, b->name);
}

static int compByPatronymic(const human_t *a, const human_t *b)
{
    return strcmp(a->patronymic, b->patronymic);
}

static int compByTelephoneNumber(const human_t *a, const human_t *b)
{
    return ((a->telephoneNumber < b->telephoneNumber) ? -1 : (a->telephoneNumber == b->telephoneNumber ? 0 : 1));
}

int comparator(const human_t *a, const human_t *b)
{
    static const COMP_POINTER comps[4] = {
            compBySurname,
            compByName,
            compByPatronymic,
            compByTelephoneNumber,
    };

    for (int i = 0; i < 4; ++i) {
        int res = comps[i](a, b);
        if (res != 0) {
            return res;
        }
    }

    return 0;
}