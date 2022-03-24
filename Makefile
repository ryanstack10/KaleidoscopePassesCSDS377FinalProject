# Change these to change the clang++/llvm-config executable
CLANG := clang++-10
LLVM := llvm-config-10

CLANG_ARGS := -g -O3
LLVM_ARGS := --cxxflags --ldflags --system-libs --libs all

# To run manually:
# clang++-10 -g -O3 toy.cpp `llvm-config-10 --cxxflags --ldflags --system-libs --libs all` -o toy
toy: toy.cpp
	$(CLANG) $(CLANG_ARGS) $< `$(LLVM) $(LLVM_ARGS)` -o $@

# To run manually:
# clang++-10 main.cpp output.o -o main
main: main.cpp output.o
	$(CLANG) $^ -o $@
