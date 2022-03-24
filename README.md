# Kaleidoscope Passes

## Overview of files
`toy.cpp` contains the Kaleidoscope source code.
`file` refers to an arbitrary file input for the Kaleidoscope parser.
`main.cpp` refers to code written by the user that uses functions defined in `file`

## Compile toy.cpp
`$ clang++-10 -g -O3 toy.cpp \`llvm-config-10 --cxxflags --ldflags --system-libs --libs all\` -o toy`
Note: clang++/llvm-config executable names may or may not include the "-10".

## Parse with toy
`$ cat file | ./toy`
This will pipe the contents of `file` into `toy` to be parsed.
The .o object file will be written to `output.o`

### parse.sh
For convenience, this script will automatically parse multiple Kaleidoscope inputs.
Usage: `$ sh parse.sh file1 [file2 ...]`
The script will automatically rename the output .o files to `file1.o file2.o ...`
Note that to use these with main.cpp, you must replace `output.o` with `file1.o` when compiling main.

## Compiling main
`$ clang++-10 main.cpp output.o -o main`
`main.cpp` contains a main function that will call the functions defined in `output.o`

## Running the code
`$ ./main`

## Syntax
Suppose we define a function `foo(a1, a2, ..., an)` in file which is parsed and encoded in `output.o`.
Now we want to use it in `main.cpp`.
We must add the following to `main.cpp`:
```
extern "C" {
	double foo(double, double, ..., double)
}
```
where `double` is repeated n times.
Now we can call `foo(-1, 3, ..., 93)`, for example.
