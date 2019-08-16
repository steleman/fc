#ifndef FC_DEBUG_H
#define FC_DEBUG_H

#include "llvm/Support/Error.h"

#define FC_DEBUG_OUT

#ifdef FC_DEBUG_OUT
#define FC_DEBUG(X) X
#else
#define FC_DEBUG(X)
#endif

namespace fc {
llvm::raw_ostream &debug();

llvm::raw_ostream &error();
} // namespace fc

#endif