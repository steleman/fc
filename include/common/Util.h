#ifndef FC_UTIL_H
#define FC_UTIL_H

#include "llvm/ADT/StringRef.h"

namespace fc {
class Util {
public:
  static long int strToInt(llvm::StringRef ref) { return std::stol(ref.str()); }
};
} // namespace fc

#endif