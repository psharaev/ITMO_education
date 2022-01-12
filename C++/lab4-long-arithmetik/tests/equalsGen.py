readFile = "5_input.txt"
writeFile = "5_expected.txt"


def kek(res):
    if res:
        return "1"
    else:
        return "0"


def getAllTest(appendFile, op):
    app = open(appendFile, "a")
    for i in range(-150, 151):
        for j in range(-150, 151):
            app.write(str(i) + " " + str(j) + " " + op + "\n")
    app.close()


def main():
    global readFile, writeFile
    inp = open(readFile, "r")
    stack = []
    for line in inp:
        a, b, op = line.split()
        if a == "NaN" or b == "NaN":
            if op == "!=":
                stack.append("1")
            elif op == ">" or op == ">=" or op == "<" or op == "<=" or op == "==":
                stack.append("0")
            elif op == "+" or op == "-" or op == "*" or op == "/" or op == "%":
                stack.append("NaN")
            continue
        a, b = int(a), int(b)
        if op == "+":
            stack.append(str(a + b))
        elif op == "-":
            stack.append(str(a - b))
        elif op == "*":
            stack.append(str(a * b))
        elif op == "/":
            if b == 0:
                stack.append("NaN")
            else:
                stack.append(str(a / b))
        elif op == "%":
            if b == 0:
                stack.append("NaN")
            else:
                stack.append(str(a % b))
        elif op == "<":
            stack.append(kek(a < b))
        elif op == "<=":
            stack.append(kek(a <= b))
        elif op == ">":
            stack.append(kek(a > b))
        elif op == ">=":
            stack.append(kek(a >= b))
        elif op == "==":
            stack.append(kek(a == b))
        elif op == "!=":
            stack.append(kek(a != b))
    inp.close()
    outp = open(writeFile, "w")
    while len(stack) > 0:
        x = stack.pop()
        outp.write(x)
        outp.write("\n")
    outp.close()


getAllTest(readFile, "%")
main()
