#include "sema/Intrinsics.h"

namespace fc {
namespace intrin {
IntrinsicKind getIntrinsicKind(llvm::StringRef ref) {
  return llvm::StringSwitch<IntrinsicKind>(ref)
#define INTRINSIC(X) .CaseLower(#X, intrin::X)
#include "sema/Intrinsics.def"
      .Default(none);
}

bool isIntrinsic(llvm::StringRef ref) {
  return (getIntrinsicKind(ref) != intrin::none);
}

} // namespace intrin
} // namespace fc
