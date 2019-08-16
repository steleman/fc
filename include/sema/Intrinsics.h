#ifndef FC_INTRIN_H
#define FC_INTRIN_H

#include "llvm/ADT/StringSwitch.h"

namespace fc {

namespace intrin {

enum IntrinsicKind {
#define INTRINSIC(X) X,
#include "sema/Intrinsics.def"
  NUM_INTRINS
#undef INTRINSIC
};

IntrinsicKind getIntrinsicKind(llvm::StringRef ref);

bool isIntrinsic(llvm::StringRef ref);

} // namespace intrin

} // namespace fc

#endif