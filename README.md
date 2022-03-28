# Kaleidoscope Passes
This repository extends chapter 8 of LLVM's Kaleidoscope tutorial to allow easy implementation of optimization passes.  
LLVM Kaleidoscope tutorial: https://llvm.org/docs/tutorial/

## Requirements
`clang 12/13`    
`llvm 12/13`

## Compile toy.cpp
`toy.cpp` contains the modified Kaleidoscope ch. 8 source code.  
```
$ clang++ -g -O3 toy.cpp `llvm-config --cxxflags --ldflags --system-libs --libs all` -o toy
```

## Parse with toy
Suppose `file` contains Kaleidoscope code.
```
$ cat file | ./toy
```
This will pipe the contents of `file` into `toy` to be parsed.  
The .o object file will be written to `output.o`

### parse.sh
For convenience, `parse.sh` will automatically parse multiple Kaleidoscope inputs.  
Usage:
```
$ sh parse.sh file1 [file2 ...]
```
The script will automatically rename the output .o files to `file1.o file2.o ...`  
To use these with main.cpp, you must include `file#.o` when compiling main.

## Compiling with Object Code
Suppose `main.cpp` contains C++ code that uses functions defined in `output.o`  
```
$ clang++ main.cpp output.o -o main
```

## Running the code
```
$ ./main
```

## Using Object Code
Suppose we define a Kaleidoscope function `foo(a1, a2, ..., an)` in `file` which is parsed and encoded in `output.o`.  
Now we want to use it in `main.cpp`.  
We must add the following to `main.cpp`:  
```
extern "C" {
    double foo(double, double, ..., double);
}
```
where `double` is repeated n times.  
Now we can call `foo(-1, 3, ..., 93)`, for example.
We can delcare as many functions inside the extern as we need.

## Example
`ops_test.txt` contains Kaleidoscope functions that test the `==, !=, <, >, <=, >=, !,` and `-` operators.  
`ops_test.cpp` contains C++ code to run the functions in `ops_test.txt`.  
(1) ``$ clang++ -g -O3 toy.cpp `llvm-config --cxxflags --ldflags --system-libs --libs all` -o toy``  
(2) `$ cat ops_test.txt | ./toy`  
(3) `$ clang++ ops_test.cpp output.o -o ops_test`  
OR  
(2) `$ sh parse.sh ops_test.txt`  
(3) `$ clang++ ops_test.cpp ops_test.o -o ops_test`  
(4) `$ ./ops_test`  
Manually verify that the operation results are correct.

## Implementing Passes
In `toy.cpp`, there is a function called `MainLoop()`. This function parses `stdin` and produces an AST. This AST is stored in a global `ModuleAST` variable called `TheModuleAST`. The `ModuleAST` contains all externs and functions defined in the parsed input. Implement passes by traversing and modifying the AST.  
When your pass is complete, call `TheModuleAST->codegen()` to generate LLVM assembly code. The code will automatically print to the terminal.  
Finally, call `GenObjectCode()` to write the LLVM assembly code to `output.o` as object code.
