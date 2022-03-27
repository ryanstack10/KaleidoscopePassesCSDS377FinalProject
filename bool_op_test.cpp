#include <iostream>

extern "C" {
    double eq(double, double);
    double ne(double, double);
    double lt(double, double);
    double gt(double, double);
    double le(double, double);
    double ge(double, double);
}

std::string b(double d) {
    return d == 0 ? "false" : "true";
}

void run(std::string op, double (*f)(double, double)) {
    std::cerr << op << ":" << std::endl;
    int y = 10;
    int arr[3] = {y-1, y, y+1};
    for (int x : arr) {
        std::cerr << x << " " << op << " " << y << " -> " << b(f(x, y)) << std::endl;
    }
}

int main(int argc, char* argv[]) {
    run("==", &eq);
    run("!=", &ne);
    run("<", &lt);
    run(">", &gt);
    run("<=", &le);
    run(">=", &ge);
    return 0;
}
