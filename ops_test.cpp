#include <iostream>

extern "C" {
    double eq(double, double);
    double ne(double, double);
    double lt(double, double);
    double gt(double, double);
    double le(double, double);
    double ge(double, double);
    double inv(double);
    double neg(double);
}

std::string b(double d) {
    return d == 0 ? "false" : "true";
}

void run(std::string op, double (*f)(double, double)) {
    std::cerr << op << ":" << std::endl;
    int y = 10;
    int arr[3] = {y-1, y, y+1};
    for (int x : arr) {
        std::cerr << x << " " << op << " " << y << " => " << b(f(x, y)) << std::endl;
    }
}

int main(int argc, char* argv[]) {
    run("==", &eq);
    run("!=", &ne);
    run("<", &lt);
    run(">", &gt);
    run("<=", &le);
    run(">=", &ge);

    std::cerr << "!:" << std::endl;
    int arr[2] = {0, 1};
    for (int x : arr) {
        std::cerr << "!(" << b(x) << ") = " << b(inv(x)) << std::endl;
    }
    std::cerr << "-:" << std::endl;
    arr[0] = 10; arr[1] = -10;
    for (int x : arr) {
	    std::cerr << "-(" << x << ") = " << neg(x) << std::endl;
    }

    return 0;
}
