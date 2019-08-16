#ifndef FC_TOKEN_H
#define FC_TOKEN_H

#include "common/Source.h"
#include "lex/TokenKinds.h"
#include "llvm/ADT/StringRef.h"

namespace fc {

using namespace tok;

// Represent the current token lexer has moved to.
struct Token {
  TokenKind Kind{tok::unknown};
  const char *value{nullptr};
  unsigned size{0};
  SourceLoc loc;
  // Holds the type size mentioned in the constant.
  int constantTypeSize{-1};

  inline llvm::StringRef getRef() const {
    assert(size >= 0);

    return llvm::StringRef(value, size);
  }
};
} // namespace fc
#endif
