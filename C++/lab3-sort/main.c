#include <stdio.h>

#include "human.h"
#include "vector.h"

int main(int argc, char *argv[])
{
    if (argc != 3) {
        fprintf(stderr, "wrong arguments count\n");
        return 1;
    }

    FILE *inFile = fopen(argv[1], "r");
    if (inFile == NULL) {
        fprintf(stderr, "error open input file to read: %s\n", argv[1]);
        return 1;
    }

    FILE *outFile = fopen(argv[2], "w");
    if (outFile == NULL) {
        fprintf(stderr, "error open output file to write: %s\n", argv[2]);
        fclose(inFile);
        return 1;
    }

    vector_t humans = createVector(16);
    if (humans.errorCode == 2) {
        fprintf(stderr, "error memory allocate\n");
        fclose(inFile);
        fclose(outFile);
        return 2;
    }

    while (!feof(inFile)) {
        human_t t;
        if (fscanf(inFile, "%s%s%s%lli", t.surname, t.name, t.patronymic, &t.telephoneNumber) != 4) {
            break;
        }
        pushBack(&humans, &t);
        if (humans.errorCode == 2) {
            fprintf(stderr, "error memory allocate for resize array\n");
            free_vector_t(humans);
            fclose(inFile);
            fclose(outFile);
            return 2;
        }

    }

    quickSort(&humans);

    if (humans.errorCode == 2) {
        fprintf(stderr, "error memory allocate in the sorting process\n");
        free_vector_t(humans);
        fclose(inFile);
        fclose(outFile);
        return 2;
    }

    for (int i = 0; i < humans.size; ++i) {
        human_t t = humans.arr[i];
        fprintf(outFile, "%s %s %s %lli\n", t.surname, t.name, t.patronymic, t.telephoneNumber);
    }

    free_vector_t(humans);
    fclose(inFile);
    fclose(outFile);

    return 0;
}