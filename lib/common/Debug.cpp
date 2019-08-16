
#include "common/Debug.h"

#include "llvm/Support/CommandLine.h"

namespace fc {

llvm::raw_null_ostream nullStr;

llvm::cl::opt<bool> Debug("fc-debug",
                          llvm::cl::desc("Print debug output messages"),
                          llvm::cl::init(false));

llvm::raw_ostream &debug() {
  if (Debug)
    return llvm::errs() << "[FC DEBUG]: ";

  return nullStr;
}

llvm::raw_ostream &error() { return llvm::errs(); }
} // namespace fc