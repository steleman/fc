#ifndef PC_DIAGNOSTICS_H
#define PC_DIAGNOSTICS_H

#include "llvm/ADT/StringRef.h"

// Hanling error warning etc.
namespace fc {

struct SourceLoc;

namespace diag {

enum ErrorKind {
#define ERROR(X, Y) X,
#include "common/messages.def"
  NUM_ERRORS
};
} // namespace diag

class Diagnostics {
  llvm::StringRef filename;
  bool errorSet;

public:
  explicit Diagnostics(llvm::StringRef name)
      : filename(name), errorSet(false) {}
  void printError(SourceLoc loc, diag::ErrorKind kind);
  bool hasError() { return errorSet; }
};
} // namespace fc
#endif