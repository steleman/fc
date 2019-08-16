#include "common/Diagnostics.h"
#include "common/Debug.h"
#include "common/Source.h"

using namespace fc;

static const char *getErrorMessage(diag::ErrorKind kind) {
  switch (kind) {
#define ERROR(X, Y)                                                            \
  case diag::X:                                                                \
    return #Y;
#include "common/messages.def"
  default:
    llvm_unreachable("unknown error");
  };
  return "";
}

static std::string getCurrLineStr(SourceLoc &loc) {
  auto temp = &loc.currStartOfLine[1];
  std::string str;
  while (temp[0] != '\n' && temp[0] != EOF) {
    str += temp[0];
    ++temp;
  }
  str = "\n\t" + str + "\n\t";
  str.append(loc.Col - 1, ' ');
  str.append("^");
  return str;
}

void Diagnostics::printError(SourceLoc loc, diag::ErrorKind kind) {

  errorSet = true;
  error() << "\n"
          << filename.str() << ":" << loc.Line << ":" << loc.Col
          << ": error: " << getErrorMessage(kind);

  error() << getCurrLineStr(loc);
}
