import os.path
from datetime import datetime


def cmp(f1, f2):
    with open(f1) as f1:
        with open(f2) as f2:
            s1 = f1.read().split()
            s2 = f2.read().split()
            n, m = len(s1), len(s2)
            if n != m:
                return [-1, -1]
            countTrue = 0
            for i in range(n):
                if s1[i] == s2[i]:
                    countTrue += 1
            return [countTrue, n]


def genFiles(count):
    global expectedMask, inputMask
    for i in range(1, count + 1):
        if not os.path.isfile(str(i) + expectedMask):
            open(str(i) + expectedMask, "x").close()
        if not os.path.isfile(str(i) + inputMask):
            open(str(i) + inputMask, "x").close()


pathBin = "C:/Users/mail/Desktop/laboratornaya-rabota-4-dlinnaya-arifmetika-pechhenka/a.exe"

expectedMask = "_expected.txt"
inputMask = "_input.txt"
actualMask = "_actual.txt"
# genFiles(15)

i = 1
count = 0
passed = 0
startTime = datetime.now()
print("\033[33m {}".format("=" * 14 + " start testing " + "=" * 14))
while True:
    if not os.path.isfile(str(i) + inputMask):
        break

    inpName = str(i) + inputMask
    actualName = str(i) + actualMask
    expectedName = str(i) + expectedMask

    print("\033[0m {}".format("Run test: " + str(i)), end=' ')
    if os.path.exists(actualName):
        os.remove(actualName)
    if not os.path.isfile(expectedName):
        print("Fail: not found expected file")

    os.system(pathBin + " " + inpName + " " + actualName)

    try:
        completed, runed = cmp(actualName, expectedName)
        if completed == -1 and runed == -1:
            print("\033[31m {}".format(
                " " * (10 - len(str(i))) + "Fail: mismatch in the number of lines in outputs"))
        elif completed == runed:
            print("\033[32m {}".format(" " * (10 - len(str(i))) + "success"))
            passed += 1
            os.remove(actualName)
        else:
            print("\033[31m {}".format(
                " " * (10 - len(str(i))) + "fail " + str(completed) + " / " + str(runed) + " <=> " + str(
                    int(completed / runed * 100)) + "%"))
    except FileNotFoundError:
        print("Fail: not found actual file")

    i += 1
    count += 1

endTime = datetime.now()

print("\033[33m {}".format("=" * 44))
print("\033[33m {}".format(f"Test all: {count}, Test passed: {passed}, Test fail: {count - passed}"))
print("\033[33m {}".format("Start time: " + startTime.strftime("%H:%M:%S")))
print("\033[33m {}".format("End time: " + endTime.strftime("%H:%M:%S")))
delta = endTime - startTime
print("\033[33m {}".format("Delta time: " + str(delta)))
print("\033[33m {}".format("=" * 44))
if count == passed:
    print("\033[32m {}".format("SUCCESS!"))
else:
    print("\033[31m {}".format("FAIL :("))
