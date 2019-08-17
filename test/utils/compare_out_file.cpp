// Copyright (c) 2019, Compiler Tree Technologies Pvt Ltd.
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice, this
//    list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Error.h"
#include <fstream>
#include <string>

static const double ERROR = 10e-4;

static void printMismatch(std::string &str1, std::string &str2) {
  llvm::errs() << "\n mismatch at: " << str1 << " " << str2 << "\n";
}

// TODO Effient way to do this ?
static bool getAsDouble(const std::string &str, double &d) {
  try {
    d = std::stod(str);
  } catch (...) {
    return false;
  }
  return true;
}

int main(int argc, char **argv) {
  // Read the file to a string.
  if (argc != 3) {
    llvm::errs() << "\n usage : compare_out_file <input-file1> <input-file2>\n";
    return 1;
  }
  std::ifstream t1(argv[1]);
  if (!t1.is_open()) {
    llvm::errs() << "\n Input file1 not found! \n";
    return 1;
  }
  std::ifstream t2(argv[2]);
  if (!t2.is_open()) {
    llvm::errs() << "\n Input file2 not found! \n";
    return 2;
  }

  while (!t1.eof() && !t2.eof()) {
    std::string str1, str2;
    t1 >> str1;
    t2 >> str2;
    llvm::StringRef s1{str1};
    llvm::StringRef s2{str2};
    if (str1 == str2)
      continue;

    // Try parsing as double.
    double d1, d2;
    bool isDouble1, isDouble2;
    isDouble1 = getAsDouble(str1, d1);
    isDouble2 = getAsDouble(str2, d2);

    if (!isDouble1 || !isDouble2) {
      printMismatch(str1, str2);
      return 1;
    }

    if (std::abs(d1 - d2) < ERROR)
      continue;

    printMismatch(str1, str2);
    return 1;
  }
  if (!t1.eof() || !t2.eof()) {
    llvm::errs() << "File size mismatch!"
                 << "\n";
    return 1;
  }
  return 0;
}
