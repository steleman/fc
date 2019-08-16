#ifndef FC_SOURCE_H
#define FC_SOURCE_H

#include <iostream>
#include <string>

namespace fc {

enum Standard {
  None = 0,
  f77 = 1,
  f95,
};

struct SourceLoc {
  unsigned Line{1};
  unsigned Col{1};
  const char *currStartOfLine{nullptr};
};
} // namespace fc
#endif
