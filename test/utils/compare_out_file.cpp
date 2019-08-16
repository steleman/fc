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
