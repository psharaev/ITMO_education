import os

test = [
    ["A", (1, 1)],
    ["B", (1, 1)],
    ["A&B", (1, 3)],
    ["A|B", (3, 1)],
    ["A->B", (3, 1)],
    ["!A", (1, 1)],
    ["!!A", (1, 1)],

    ["A->B->A", (4, 0)],
    ["(A->B)->(A->B->C)->(A->B)", (8, 0)],
    ["A&B->A", (4, 0)],
    ["A&B->B", (4, 0)],
    ["A->B->A&B", (4, 0)],
    ["A->A|B", (4, 0)],
    ["B->A|B", (4, 0)],
    ["(A->C)->(B->C)->(A|B->C)", (8, 0)],
    ["(A->B)->(A->!B)->(!A)", (4, 0)],
    ["(A->B)->(A->!B)->!A", (4, 0)],
    ["!!A->A", (2, 0)],

    ["((A))", (1, 1)],
    ["!((A))", (1, 1)],
    ["!A|B", (3, 1)],

    ["A | A |\tA", (1, 1)],
    ["A & A &A", (1, 1)],
    ["!(A&!B)", (3, 1)],
]

count_success = 0
count_failed = 0


def assertEqual(actual, line, expect):
    global count_success, count_failed
    if actual == expect:
        count_success += 1
    else:
        count_failed += 1
        print(f"Failed test: {line}, actual: {actual}")


for line in test:
    with open("inp.txt", "w") as inp:
        inp.write(line[0])
    os.system("./Main < inp.txt > out.txt")
    with open("out.txt", "r") as out:
        s = out.readline()
        t, f = line[1]
        if t == 0:
            assertEqual(s, line, "Unsatisfiable")
        elif f == 0:
            assertEqual(s, line, "Valid")
        else:
            assertEqual(s, line, f"Satisfiable and invalid, {t} true and {f} false cases")

os.remove("inp.txt")
os.remove("out.txt")
print(f"{count_success=} {count_failed=}")
