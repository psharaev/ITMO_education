#include <iostream>
#include <string>
#include <fstream>
#include <stack>
#include "LN.h"

void dumpResult(std::ostream &outp, std::stack<LN> &s);

int main(int argc, char *argv[])
{
    if (argc != 3) {
        std::cerr << "wrong arguments count\nexpected two arguments: path to input file and path to output file"
                  << std::endl;
        return 1;
    }

    std::ifstream inp;
    inp.open(argv[1]);
    if (!inp.is_open()) {
        std::cerr << "fail open input file: " << argv[1] << std::endl;
        return 1;
    }

    std::ofstream outp;
    outp.open(argv[2]);
    if (!outp.is_open()) {
        std::cerr << "fail open output file: " << argv[2] << std::endl;
        return 1;
    }

    std::string t;
    std::stack<LN> s;
    try {
        while (inp >> t) {
            if (t == "+") {
                LN b = s.top();
                s.pop();
                LN a = s.top();
                s.pop();
                s.emplace(a + b);
            } else if (t == "-") {
                LN b = s.top();
                s.pop();
                LN a = s.top();
                s.pop();
                s.emplace(a - b);
            } else if (t == "*") {
                LN b = s.top();
                s.pop();
                LN a = s.top();
                s.pop();
                s.emplace(a * b);
            } else if (t == "/") {
                LN b = s.top();
                s.pop();
                LN a = s.top();
                s.pop();
                s.emplace(a / b);
            } else if (t == "%") {
                LN b = s.top();
                s.pop();
                LN a = s.top();
                s.pop();
                s.emplace(a % b);
            } else if (t == "~") {
                LN a = s.top();
                s.pop();
                s.emplace(a.sqrt());
            } else if (t == "_") {
                LN a = s.top();
                s.pop();
                s.emplace(-a);
            } else if (t == "<") {
                LN b = s.top();
                s.pop();
                LN a = s.top();
                s.pop();
                s.emplace(a < b);
            } else if (t == "<=") {
                LN b = s.top();
                s.pop();
                LN a = s.top();
                s.pop();
                s.emplace(a <= b);
            } else if (t == ">") {
                LN b = s.top();
                s.pop();
                LN a = s.top();
                s.pop();
                s.emplace(a > b);
            } else if (t == ">=") {
                LN b = s.top();
                s.pop();
                LN a = s.top();
                s.pop();
                s.emplace(a >= b);
            } else if (t == "==") {
                LN b = s.top();
                s.pop();
                LN a = s.top();
                s.pop();
                s.emplace(a == b);
            } else if (t == "!=") {
                LN b = s.top();
                s.pop();
                LN a = s.top();
                s.pop();
                s.emplace(a != b);
            } else {
                s.emplace(t);
            }
        }
    } catch (std::bad_alloc &e) {
        std::cerr << "failed to allocate memory while counting\ndumping partial result..." << std::endl;
        inp.close();
        dumpResult(outp, s);
        outp.close();
        return 2;
    }

    inp.close();
    dumpResult(outp, s);
    outp.close();

    return 0;
}

void dumpResult(std::ostream &outp, std::stack<LN> &s)
{
    while (!s.empty()) {
        outp << s.top() << std::endl;
        s.pop();
    }
}
