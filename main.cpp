#include <iostream>

extern "C" {
	double foo(double);
}

int main(int argc, char* argv[]) {
	std::cerr << foo(9) << std::endl;
	return 0;
}
