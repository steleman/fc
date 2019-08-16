# **FC - LLVM Based fortran frontend**

A new front end for Fortran has been written in the design spirit of LLVM/Clang. Approximately 20000 lines of code effort has got close to Fortran 95 standard. The Front end is complete end to end solution without semantic analysis. It compiles 400+ unit test cases and 2 SPEC CPU 2017 benchmarks.

### Dependencies
```
LLVM 7.0 - Can be built from source or prebuilt binaries can be used.
gfortran (for running unit tests)
```

## Build instructions
```
1. Set following environment variables
    a. CLANG_BINARY=<path/to/llvm/instal>bin/clang
    b. PATH=<path/to/llvm/install/>/bin:$PATH
    c. LD_LIBRARY_PATH=<path/to/install/>/lib;$PATH
2. mkdir build && cd build
3. cmake -G Ninja -DLLVM_DIR=<path/to/llvm>/lib/cmake/llvm/ ../fc -DCMAKE_INSTALL_PREFIX=<install-prefix> -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++
4. ninja install
5. ninja test


Note that clang-7.0 is already in your PATH. This will be used for building FC.
```

## Running HelloWorld

```
$ cat hello.f90
program hello
  print *, "Hello world!"
end program hello
$ export CLANG_BINARY=<path/to/llvm/install>/bin/clang
$ export LD_LIBRARY_PATH=<path/to/fc/install>/lib:$LD_LIBRARY_PATH
$ <path/to/fc>/bin/fc hello.f90
$ ./a.out
Hello world!
```

## Running test

```
cd build
ctest 
```
OR

```
cd build
ninja test
```

## Compiling input file to AST

```
fc <inputfile> -emit-ast
```
OR
```
fc <inputfile> -emit-ast -o <ast_output_file>
```

## Emitting LLVM IR
```
fc <inputfile> -emit-llvm
```

## Emitting LLVM BC

```
fc <inputfile> -emit-bc
```

## Fortran standard 
Fortran  standard can be specified via -std=f77 or -std=f95. If not specified standard is derived from file extension. Source files with ".f" are compiled to F77 standard and all other extensions are compiled to default standard.

### Enter following command for more options
```
fc --help
```

## Developed by
[CompilerTree Technologies](http://compilertree.com)
